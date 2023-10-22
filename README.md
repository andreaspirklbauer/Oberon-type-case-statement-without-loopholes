# Oberon-type-case-statement-without-loopholes
Type case statement without type loopholes for the Oberon-07 programming language

Note: In this repository, the term "Project Oberon 2013" refers to a re-implementation of the original "Project Oberon" on an FPGA development board around 2013, as published at www.projectoberon.com.

**PREREQUISITES**: A current version of Project Oberon 2013 (see http://www.projectoberon.com).

------------------------------------------------------

This repository modifies the semantics of the *type* case statement of Oberon-07

    CASE x OF
     | T1: S1
     | T2: S2
     | T3: S3
    END

such that the following rules and restrictions apply:

* The case variable must be a *simple* identifier that cannot be followed by a selector, i.e. it cannot be an element of a structure (array element or record field).

* The case variable must be either a *local* variable or *value* parameter of pointer type (pointing to a record) or a *variable parameter* of record type.

* A case variable of pointer type cannot be assigned a different value or passed as a *variable* parameter to a procedure within the scope of the type case statement. However, individual elements (fields) of a case variable may be modified or passed as variable parameters.

This eliminates a number of type loopholes. See the test program [**TestTypeCase.Mod**](Sources/FPGAOberon2013/TestTypeCase.Mod).

------------------------------------------------------
**Preparing your system to use the modified Oberon compiler**

If *Project Oberon 2013* is used, follow the instructions below:

------------------------------------------------------

Convert the downloaded files to Oberon format (Oberon uses CR as line endings) using the command [**dos2oberon**](dos2oberon), also available in this repository (example shown for Mac or Linux):

     for x in *.Mod ; do ./dos2oberon $x $x ; done

Import the files to your Oberon system. If you use an emulator (e.g., **https://github.com/pdewacht/oberon-risc-emu**) to run the Oberon system, click on the *PCLink1.Run* link in the *System.Tool* viewer, copy the files to the emulator directory, and execute the following command on the command shell of your host system:

     cd oberon-risc-emu
     for x in *.Mod ; do ./pcreceive.sh $x ; sleep 0.5 ; done

Compile the downloaded files as follows:

     ORP.Compile ORG.Mod/s ORP.Mod/s ~  # build the MODIFIED compiler
     System.Free ORP ORG ~              # unload the ORIGINAL compiler

     ORP.Compile TestTypeCase.Mod/s ~   # compile the test program with the MODIFIED compiler

------------------------------------------------------
**Implementation**

First, we implement the two rules

* The case variable must be a *simple* identifier that cannot be followed by a selector, i.e. it cannot be an element of a structure (array element or record field).

* The case variable must be either a *local* variable or *value* parameter of pointer type (pointing to a record) or a *variable parameter* of record type.

The first rule is implemented by simply *not* calling *ORP.selector* after calling *qualident* when the case variable is parsed. The second rule is implemented by adding the check

    IF (obj.lev <= 0) OR
      (obj.type.form = ORB.Pointer) & ((obj.class # ORB.Var) OR (obj.type.base.form # ORB.Record)) OR
      (obj.type.form = ORB.Record) & (obj.class # ORB.Par)

to procedure *ORP.StatSequence*, which now reads:

    PROCEDURE StatSequence;
      ...
      ELSIF sym = ORS.case THEN  (*case statement*)
        ORS.Get(sym);
        IF sym = ORS.ident THEN
          qualident(obj);  (*no call to selector here*)
          IF (obj.type.form IN {ORB.Pointer, ORB.Record}) THEN  (*type case statement*)
            IF (obj.type.form = ORB.Pointer) & ((obj.class # ORB.Var) OR (obj.type.base.form # ORB.Record)) OR
              (obj.type.form = ORB.Record) & (obj.class # ORB.Par) OR (obj.lev <= 0)
            THEN ORS.Mark("invalid case expression")
            END ;
            TypeCasePart(obj)

Second, we implement the rule 

* A case variable of pointer type cannot be assigned a different value or passed as a *variable* parameter to a procedure within the scope of the type case statement. However, individual elements (fields) of a case variable may be modified or passed as variable parameters.

To do so, we need a way to detect whether any object *obj* encountered during compilation is in fact a case variable of a type case statement.

Recall that in the official Oberon-07 compiler, the following invariant holds for all declared objects *obj* during compilation:

    obj.lev > 0  =>  ~obj.expo & (obj.exno = 0)

This simply means that if an object is a *local* variable or a formal *parameter*, it cannot have an export mark (asterisk).

Since the case variable of a *type* case statement must be either a *local* variable or procedure *parameter* (*obj.lev > 0* always), we can "abuse" either the field *obj.expo* or the field *obj.exno* (or both) to indicate that we are *inside* a type case statement.

For example, we may define

    (obj.lev > 0) & (obj.exno > 0)  =>  we are inside a type case statement (and obj is the case variable)

and let the code implementing the type case statement temporarily set *obj.exno* to a value different that 0.

Recall that the *type* case statement represents the singular case where a symbol table entry - the type of *obj* - is temporarily modified during compilation (but only until the end of the statement, where the change is reverted). With the above change, we add another temporary modification to the (same) symbol table entry (namely the field *obj.exno*).

In order to handle *nested* type case statements correctly, we increase the value of *obj.exno* by 1 when entering a type case statement and decrease it by 1 when exiting it, i.e. the current value of the field *obj.exno* effectively represents the nesting level of type case statements.

    PROCEDURE Sample();
      VAR p: P0;       (*obj is the local variable p => obj.lev = 1 & ~obj.expo & obj.exno = 0*)
    BEGIN
      p := p1;         (*correct, since we are outside ANY type case statements*)
      CASE p OF        (*increase obj.exno from 0 to 1*)
        P1:
          p := p2;     (*"read-only" compile-time error*)
          CASE p OF    (*increase obj.exno from 1 to 2*)
          P1:
            p := p1    (*"read-only" compile-time error*)
          END ;        (*decrease obj.exno from 2 to 1 = the value before entering the inner case statement*)
          p := p1      (*"read-only" compile-time error, since we are still inside the outer case statement*)
      END ;            (*decrease obj.exno from 1 to 0 = the value before entering the outer case statement*)
      p := p1          (*correct again, since we are now outside ANY type case statements again*)
    END Sample;

This leads to the following code:

     PROCEDURE TypeCasePart(obj: ORB.Object);
       VAR L0: LONGINT;
     BEGIN Check(ORS.of, "OF expected"); L0 := 0; INC(obj.exno);  (*<---*)
       WHILE (sym < ORS.end) OR (sym = ORS.bar) DO
         IF sym = ORS.bar THEN ORS.Get(sym) ELSE TypeCase(obj, L0) END
       END ;
       IF sym = ORS.else THEN ORS.Get(sym); StatSequence END ;
       ORG.FixLink(L0); DEC(obj.exno)  (*<---*)
     END TypeCasePart;

Since this is the only place where *obj.exno* is modified, it is easy to see that the *only* place where the condition

     (obj.lev > 0) & (obj.exno > 0)
    
can ever be true is *inside* a type case statement.

**Note:** It is absolutely necessary to reset the value of *obj.exno* back to its original value after compiling a type case statement, because it will be used during the export process (see *ORB.Export* and *ORB.OutType*, which both output the value of *obj.exno* to the symbol file).

We must also be able to recognize whether the identified case variable is a *simple* identifier that is *not* followed by a selector. To achieve this, we introduce an additional item field *x.selected* in module *ORG*,

initialise it to FALSE in ORG.MakeItem

    x.selected := FALSE

set it to TRUE in ORP.selector

     WHILE (sym = ORS.lbrak) OR (sym = ORS.period) OR (sym = ORS.arrow)
        OR (sym = ORS.lparen) & (x.type.form IN {ORB.Record, ORB.Pointer}) DO x.selected := TRUE;

and finally add the check for *x.selected* to the above condition, leading to:

    ~x.selected & (obj.lev > 0) & (obj.exno > 0)  =>  we are inside a type case statement (and obj is the case variable)

Finally, we must be able to recognize whether the case variable is of *pointer* type. This adds the condition *obj.type.form = ORP.Pointer* to the above expression. Putting all together, wo obtain:

    PROCEDURE CheckTypeCase(VAR x: ORG.Item; obj: ORB.Object);
    BEGIN
      IF ~x.selected & (obj.lev > 0) & (obj.exno > 0) & (obj.type.form = ORB.Pointer) THEN x.rdo := TRUE END
    END CheckTypeCase;

We now add this check when assignments and procedure parameters are parsed:

*1. Assignments to case variables*

    PROCEDURE StatSequence;
      ...
      IF sym = ORS.ident THEN
        qualident(obj); ORG.MakeItem(x, obj, level);
        IF x.mode = ORB.SProc THEN StandProc(obj.val)
        ELSE selector(x);
          IF sym = ORS.becomes THEN (*assignment*)
            ORS.Get(sym); CheckTypeCase(x, obj); CheckReadOnly(x); expression(y);  (*CheckTypeCase may set x.rdo*)

*2. Passing case variables as procedure parameters*

    PROCEDURE Parameter(par: ORB.Object);  (*par is the formal parameter, x the actual parameter*)
      VAR x: ORG.Item; varpar: BOOLEAN;
    BEGIN expression(x);                               (*calls ORP.factor, which calls CheckTypeCase, which in turn may set x.rdo*)
      IF par # NIL THEN
        varpar := par.class = ORB.Par;
        IF CompTypes(par.type, x.type, varpar) THEN
          IF ~varpar THEN ORG.ValueParam(x)
          ELSE (*par.class = Par, i.e VAR parameter*)
            IF ~par.rdo THEN CheckReadOnly(x) END ;    (*issues an error message if CheckTypeCase has set x.rdo*)
            ORG.VarParam(x, par.type)
          END

    PROCEDURE factor(VAR x: ORG.Item);
      ...
      IF sym = ORS.ident THEN
        qualident(obj);  
        IF obj.class = ORB.SFunc THEN StandFunc(x, obj.val, obj.type)
        ELSE ORG.MakeItem(x, obj, level); selector(x); CheckTypeCase(x, obj);  (*CheckTypeCase may set x.rdo*)

Writing the actual error message is deferred to *CheckReadOnly*. In the case of *ORP.Parameter*, this is necessary, because *ORP.factor* does *not* return the object designating the case variable. We use the field *x.rdo* to transport the result of the call to *CheckTypeCase* back to the caller.

DIFFERENCES TO PROJECT OBERON 2013

-------------------------------------------------------------------------------------


**ORG**

```diff
--- FPGAOberon2013/ORG.Mod	2019-05-30 17:58:14.000000000 +0200
+++ Oberon-type-case-statement-without-loopholes/Sources/FPGAOberon2013/ORG.Mod	2021-09-26 07:11:27.000000000 +0200
@@ -22,7 +22,7 @@
       mode*: INTEGER;
       type*: ORB.Type;
       a*, b*, r: LONGINT;
-      rdo*: BOOLEAN  (*read only*)
+      rdo*, selected*: BOOLEAN  (*read only, selected in record or array, dereferenced*)
     END ;
 
   (* Item forms and meaning of fields:
@@ -247,7 +247,7 @@
   END MakeStringItem;
 
   PROCEDURE MakeItem*(VAR x: Item; y: ORB.Object; curlev: LONGINT);
-  BEGIN x.mode := y.class; x.type := y.type; x.a := y.val; x.rdo := y.rdo;
+  BEGIN x.mode := y.class; x.type := y.type; x.a := y.val; x.rdo := y.rdo; x.selected := FALSE;
     IF y.class = ORB.Par THEN x.b := 0
     ELSIF (y.class = ORB.Const) & (y.type.form = ORB.String) THEN x.b := y.lev  (*len*) ;
     ELSE x.r := y.lev
```

**ORP**

```diff
--- FPGAOberon2013/ORP.Mod	2021-05-24 10:06:15.000000000 +0200
+++ Oberon-type-case-statement-without-loopholes/Sources/FPGAOberon2013/ORP.Mod	2021-09-26 07:18:49.000000000 +0200
@@ -87,6 +87,11 @@
     END
   END CheckExport;
 
+  PROCEDURE CheckCase(VAR x: ORG.Item; obj: ORB.Object);
+  BEGIN
+    IF ~x.selected & (obj.lev > 0) & (obj.exno > 0) & (obj.type.form = ORB.Pointer) THEN x.rdo := TRUE END
+  END CheckCase;
+
   PROCEDURE IsExtension(t0, t1: ORB.Type): BOOLEAN;
   BEGIN (*t1 is an extension of t0*)
     RETURN (t0 = t1) OR (t1 # NIL) & IsExtension(t0, t1.base)
@@ -121,7 +126,7 @@
     VAR y: ORG.Item; obj: ORB.Object;
   BEGIN
     WHILE (sym = ORS.lbrak) OR (sym = ORS.period) OR (sym = ORS.arrow)
-        OR (sym = ORS.lparen) & (x.type.form IN {ORB.Record, ORB.Pointer}) DO
+        OR (sym = ORS.lparen) & (x.type.form IN {ORB.Record, ORB.Pointer}) DO x.selected := TRUE;
       IF sym = ORS.lbrak THEN
         REPEAT ORS.Get(sym); expression(y);
           IF x.type.form = ORB.Array THEN
@@ -130,7 +135,8 @@
           END
         UNTIL sym # ORS.comma;
         Check(ORS.rbrak, "no ]")
-      ELSIF sym = ORS.period THEN ORS.Get(sym);
+      ELSIF sym = ORS.period THEN
+        ORS.Get(sym);
         IF sym = ORS.ident THEN
           IF x.type.form = ORB.Pointer THEN ORG.DeRef(x); x.type := x.type.base END ;
           IF x.type.form = ORB.Record THEN
@@ -315,7 +321,7 @@
     IF sym = ORS.ident THEN
       qualident(obj);  
       IF obj.class = ORB.SFunc THEN StandFunc(x, obj.val, obj.type)
-      ELSE ORG.MakeItem(x, obj, level); selector(x);
+      ELSE ORG.MakeItem(x, obj, level); selector(x); CheckCase(x, obj);
         IF sym = ORS.lparen THEN
           ORS.Get(sym);
           IF (x.type.form = ORB.Proc) & (x.type.base.form # ORB.NoTyp) THEN
@@ -462,21 +468,31 @@
 
   PROCEDURE StatSequence;
     VAR obj: ORB.Object;
-      orgtype: ORB.Type; (*original type of case var*)
       x, y, z, w: ORG.Item;
       L0, L1, rx: LONGINT;
 
-    PROCEDURE TypeCase(obj: ORB.Object; VAR x: ORG.Item);
-      VAR typobj: ORB.Object;
-    BEGIN
+    PROCEDURE TypeCase(obj: ORB.Object; VAR L0: LONGINT);
+      VAR typobj: ORB.Object; x: ORG.Item;
+        orgtype: ORB.Type;  (*original type of case var*)
+    BEGIN (*obj.lev > 0 & obj.exno > 0*)
       IF sym = ORS.ident THEN
-        qualident(typobj); ORG.MakeItem(x, obj, level);
+        qualident(typobj); ORG.MakeItem(x, obj, level); orgtype := obj.type;
         IF typobj.class # ORB.Typ THEN ORS.Mark("not a type") END ;
         TypeTest(x, typobj.type, FALSE); obj.type := typobj.type;
-        ORG.CFJump(x); Check(ORS.colon, ": expected"); StatSequence
-      ELSE ORG.CFJump(x); ORS.Mark("type id expected")
-      END
-     END TypeCase;
+        ORG.CFJump(x); Check(ORS.colon, ": expected"); StatSequence;
+        ORG.FJump(L0); ORG.Fixup(x); obj.type := orgtype;
+      ELSE ORS.Mark("type id expected"); Check(ORS.colon, ": expected"); StatSequence
+      END
+    END TypeCase;
+
+    PROCEDURE TypeCasePart(obj: ORB.Object);
+      VAR L0: LONGINT;
+    BEGIN Check(ORS.of, "OF expected"); L0 := 0; INC(obj.exno);
+      WHILE (sym < ORS.end) OR (sym = ORS.bar) DO
+        IF sym = ORS.bar THEN ORS.Get(sym) ELSE TypeCase(obj, L0) END
+      END ;
+      ORG.FixLink(L0); DEC(obj.exno)
+    END TypeCasePart;
 
     PROCEDURE SkipCase;
     BEGIN 
@@ -495,7 +511,7 @@
         IF x.mode = ORB.SProc THEN StandProc(obj.val)
         ELSE selector(x);
           IF sym = ORS.becomes THEN (*assignment*)
-            ORS.Get(sym); CheckReadOnly(x); expression(y);
+            ORS.Get(sym); CheckCase(x, obj); CheckReadOnly(x); expression(y);
             IF CompTypes(x.type, y.type, FALSE) THEN
               IF (x.type.form <= ORB.Pointer) OR (x.type.form = ORB.Proc) THEN ORG.Store(x, y)
               ELSE ORG.StoreStruct(x, y)
@@ -569,13 +585,13 @@
       ELSIF sym = ORS.case THEN
         ORS.Get(sym);
         IF sym = ORS.ident THEN
-          qualident(obj); orgtype := obj.type;
-          IF (orgtype.form = ORB.Pointer) OR (orgtype.form = ORB.Record) & (obj.class = ORB.Par) THEN
-            Check(ORS.of, "OF expected"); TypeCase(obj, x); L0 := 0;
-            WHILE sym = ORS.bar DO
-              ORS.Get(sym); ORG.FJump(L0); ORG.Fixup(x); obj.type := orgtype; TypeCase(obj, x)
+          x.selected := FALSE; qualident(obj);
+          IF (obj.type.form IN {ORB.Pointer, ORB.Record}) THEN
+            IF (obj.type.form = ORB.Pointer) & ((obj.class # ORB.Var) OR (obj.type.base.form # ORB.Record)) OR
+              (obj.type.form = ORB.Record) & (obj.class # ORB.Par) OR (obj.lev <= 0)
+            THEN ORS.Mark("invalid case expression")
             END ;
-            ORG.Fixup(x); ORG.FixLink(L0); obj.type := orgtype
+            TypeCasePart(obj)
           ELSE ORS.Mark("numeric case not implemented");
             Check(ORS.of, "OF expected"); SkipCase;
             WHILE sym = ORS.bar DO SkipCase END
@@ -603,12 +619,12 @@
         IF sym = ORS.ident THEN ORB.NewObj(obj, ORS.id, class); ORS.Get(sym); CheckExport(obj.expo)
         ELSE ORS.Mark("ident?")
         END
-      END ;
+      END;
       IF sym = ORS.colon THEN ORS.Get(sym) ELSE ORS.Mark(":?") END
     ELSE first := NIL
     END
   END IdentList;
-
+  
   PROCEDURE ArrayType(VAR type: ORB.Type);
     VAR x: ORG.Item; typ: ORB.Type; len: LONGINT;
   BEGIN NEW(typ); typ.form := ORB.NoTyp;
```
