# Oberon-type-case-statement-without-loopholes
Type case statement without type loopholes for the Oberon-07 programming language

Note: In this repository, the term "Project Oberon 2013" refers to a re-implementation of the original "Project Oberon" on an FPGA development board around 2013, as published at www.projectoberon.com.

**PREREQUISITES**: A current version of Project Oberon 2013 (see http://www.projectoberon.com) or Extended Oberon (see http://github.com/andreaspirklbauer/Oberon-extended)

Note: On Project Oberon 2013, this implementation is a superset of http://github.com/andreaspirklbauer/Oberon-numeric-case-statement

------------------------------------------------------

This repository modifies the semantics of the *type* case statement of Oberon-07

    CASE x OF
       T1: S1
     | T2: S2
     | T3: S3
    END

such that the following rules and restrictions apply:

* **Restriction #1:** The case variable must be a *simple* identifier that cannot be followed by a selector for structured variables (e.g., an array element, a record field, or a type guard).

* **Restriction #2:** The case variable must be either a *local* variable or *value* parameter of pointer type (pointing to a record) or a *variable parameter* of record type.

* **Restriction #3:** A case variable of pointer type cannot be assigned a different value or passed as a *variable* parameter to a procedure within the scope of the type case statement. However, individual elements (fields) of a case variable may be modified or passed as variable parameters.

This eliminates a number of type loopholes. See the test program [**TestTypeCase.Mod**](Sources/FPGAOberon2013/TestTypeCase.Mod).

------------------------------------------------------

**Preparing your compiler to support the modified case statement without loopholes**

**STEP 1**: On *Extended Oberon*, skip this step. On Project Oberon 2013, build a slightly modified compiler as follows:

Edit the file *ORG.Mod* and set the following constants to the indicated new values:

    CONST ...
      maxCode = 8800; maxStrx = 3200; ...

Then recompile the modified file *ORG.Mod* and unload the old compiler:

    ORP.Compile ORG.Mod/s ~
    System.Free ORTool ORP ORG ~

This step is (unfortunately) necessary since the Oberon-07 compiler has a tick too restrictive constants. To compile the new version of the Oberon-07 compiler, one needs slightly more space (in the compiler) for both *code* and *string constants*.

------------------------------------------------------

**STEP 2**: Download and import the files to implement the modified case statement to your system

On Project Oberon 2013, download all files from the [**Sources/FPGAOberon2013**](Sources/FPGAOberon2013) directory of this repository. On Extended Oberon, download all files from the [**Sources/ExtendedOberon**](Sources/ExtendedOberon) directory.

Convert the *source* files to Oberon format (Oberon uses CR as line endings) using the command [**dos2oberon**](dos2oberon), also available in this repository (example shown for Linux or MacOS):

    for x in *.Mod ; do ./dos2oberon $x $x ; done

Import the files to your Oberon system. If you use an emulator, click on the *PCLink1.Run* link in the *System.Tool* viewer, copy the files to the emulator directory, and execute the following command on the command shell of your host system:

    cd oberon-risc-emu
    for x in *.Mod ; do ./pcreceive.sh $x ; sleep 0.5 ; done

------------------------------------------------------

**STEP 3:** Build the modified Oberon compiler:

On Project Oberon 2013:

    ORP.Compile ORG.Mod/s ORP.Mod/s ~

On Extended Oberon:

    ORP.Compile ORP.Mod/s ~

------------------------------------------------------

**STEP 4:** Unload the old Oberon compiler from memory:

    System.Free ORTool ORP ORG ORB ORS ~

------------------------------------------------------

**STEP 5:** Test the modified case statement:

    ORP.Compile TestTypeCase.Mod/s ~

------------------------------------------------------
**IMPLEMENTING RESTRICTIONS 1, 2 AND 3**

**Restriction #1:** (already implemented in Extended Oberon)

* The case variable must be a *simple* identifier that cannot be followed by a selector for structured variables (e.g., an array element, a record field, or a type guard).

To implement this rule, we introduce an additional item field *x.obj* in type *ORG.Item* and

a) initialise it to the object (=*y*) from which the item is created in *ORG.MakeItem*:

    x.obj := y

b) set it to NIL in *ORP.selector* if an item is followed by a selector for structured variables:

     WHILE (sym = ORS.lbrak) OR (sym = ORS.period) OR (sym = ORS.arrow)
         OR (sym = ORS.lparen) & (x.type.form IN {ORB.Record, ORB.Pointer}) DO
       IF sym = ORS.lbrak THEN ...
       ELSIF sym = ORS.period THEN ...
       ELSIF sym = ORS.arrow THEN ...
       ELSIF ...
       END ;
       x.obj := NIL  (*<---*)
     END

c) and add the condition *x.obj # NIL* to the guard for case statements in *ORP.StatSequence*:

    ELSIF (x.obj # NIL) & ... THEN TypeCasePart(x.obj) ELSE ORS.Mark("invalid case variable") ... END

------------------------------------------------------

**Restriction #2:**

* The case variable must be either a *local* variable or *value* parameter of pointer type (pointing to a record) or a *variable parameter* of record type.

This rule is implemented by adding the guard

    ELSIF ... & (x.obj.lev > 0) &
      ((x.type.form = ORB.Pointer) & (x.mode = ORB.Var) & (x.type.base.form = ORB.Record) OR
      (x.type.form = ORB.Record) & (x.mode = ORB.Par)) THEN TypeCasePart(x.obj)

in procedure *ORP.StatSequence*, which for case statements now reads as follows (together with restriction #1):

    PROCEDURE StatSequence;
      ...
      ELSIF sym = ORS.case THEN
        ORS.Get(sym); x.obj := NIL; expression(x);
        IF x.type.form IN {ORB.Int, ORB.Byte, ORB.Char} THEN NumericCasePart(x)
        ELSIF (x.obj # NIL) & (x.obj.lev > 0) &
          ((x.type.form = ORB.Pointer) & (x.mode = ORB.Var) & (x.type.base.form = ORB.Record) OR
          (x.type.form = ORB.Record) & (x.mode = ORB.Par)) THEN TypeCasePart(x.obj)
        ELSE ORS.Mark("invalid case variable"); SkipCase
        END ;
        Check(ORS.end, "no END")
      END

------------------------------------------------------

**Restriction #3:**

* A case variable of pointer type cannot be assigned a different value or passed as a *variable* parameter to a procedure within the scope of the type case statement. However, individual elements (fields) of a case variable may be modified or passed as variable parameters.

To implement this rule, we need a way to detect whether any object *obj* encountered during compilation is in fact a case variable of pointer type in a type case statement.

Recall that in the Oberon-07 compiler, the following invariant holds for all declared objects *obj* during compilation:

    obj.lev > 0  =>  ~obj.expo & (obj.exno = 0)

This simply means that if an object is a *local* variable or a formal *parameter*, it cannot have an export mark (asterisk).

Since the case variable of a *type* case statement must be either a *local* variable or procedure *parameter* (i.e. *obj.lev > 0* always), we can "abuse" either the field *obj.expo* or the field *obj.exno* or both to indicate that we are *inside* a type case statement.

For example, we may define

    (obj.lev > 0) & (obj.exno > 0)  =>  we are inside a type case statement and obj is the case variable

and let the code implementing the type case statement (*ORP.TypeCasePart*) temporarily set *obj.exno* to a value different that 0.

We recall that the *type* case statement represents the singular case where a symbol table entry (namely the type of *obj*) is temporarily modified during compilation. With the above modification, we now make another *temporary* modification to the (same) symbol table entry (namely the field *obj.exno*).

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

This leads to the following code for compiling type case statements:

    PROCEDURE TypeCasePart(obj: ORB.Object);
      VAR L0: LONGINT;
    BEGIN Check(ORS.of, "OF expected"); L0 := 0; INC(obj.exno);  (*<---*)
      WHILE (sym < ORS.end) OR (sym = ORS.bar) DO
        IF sym = ORS.bar THEN ORS.Get(sym) ELSE TypeCase(obj, L0) END
      END ;
      IF sym = ORS.else THEN ORS.Get(sym); StatSequence END ;
      ORG.FixLink(L0); DEC(obj.exno)                             (*<---*)
    END TypeCasePart;

Note: It is absolutely necessary to reset the value of *obj.exno* back to its original value after compiling a type case statement in *ORP.TypeCasePart*, because this field is also used during the export process. See procedures *ORB.Export* and *ORB.OutType*, which both output the value of *obj.exno* to the symbol file.

Since this is the only place where *obj.exno* is modified, it is easy to see that the *only* place where the condition

     (obj.lev > 0) & (obj.exno > 0)
    
can ever be true is *inside* a type case statement. We can therefore simply insert the check

    PROCEDURE CheckCase(VAR x: ORG.Item);
    BEGIN (*x.obj is a simple identifier of pointer type in a type case statement that is not followed by a selector*)
      IF (x.obj # NIL) & (x.obj.lev > 0) & (x.obj.exno > 0) & (x.obj.type.form = ORB.Pointer) THEN x.rdo := TRUE END
    END CheckCase;

whenever assignments and procedure parameters are parsed:

*1. Assignments to case variables*

    PROCEDURE StatSequence;
      ...
      IF sym = ORS.ident THEN
        qualident(obj); ORG.MakeItem(x, obj, level);
        IF x.mode = ORB.SProc THEN StandProc(obj.val)
        ELSE selector(x);
          IF sym = ORS.becomes THEN (*assignment*)
            ORS.Get(sym); CheckCase(x); CheckReadOnly(x); expression(y);  (*CheckCase may set x.rdo*)

*2. Passing case variables as procedure parameters*

    PROCEDURE Parameter(par: ORB.Object);  (*par is the formal parameter, x the actual parameter*)
      VAR x: ORG.Item; varpar: BOOLEAN;
    BEGIN expression(x);                               (*calls ORP.factor, which calls CheckCase, which in turn may set x.rdo*)
      IF par # NIL THEN
        varpar := par.class = ORB.Par;
        IF CompTypes(par.type, x.type, varpar) THEN
          IF ~varpar THEN ORG.ValueParam(x)
          ELSE (*par.class = Par, i.e VAR parameter*)
            IF ~par.rdo THEN CheckReadOnly(x) END ;    (*issues an error message if CheckCase has set x.rdo*)
            ORG.VarParam(x, par.type)
          END

    PROCEDURE factor(VAR x: ORG.Item);
      ...
      IF sym = ORS.ident THEN
        qualident(obj);  
        IF obj.class = ORB.SFunc THEN StandFunc(x, obj.val, obj.type)
        ELSE ORG.MakeItem(x, obj, level); selector(x); CheckCase(x);  (*CheckCase may set x.rdo*)

We note that writing the actual error message is deferred to *CheckReadOnly*.

In the case of *ORP.Parameter*, this is necessary, because *ORP.factor* does *not* return the object designating the case variable. We use the field *x.rdo* to transport the result of the call to *CheckCase* back to the caller.

-------------------------------------------------------------------------------------

**DIFFERENCES IN PROJECT OBERON 2013 TO THE FULL IMPLEMENTION OF THE NUMERIC CASE STATEMENT**

Differences to the repository **http://github.com/andreaspirklbauer/Oberon-numeric-case-statement**:

**ORP**

```diff
--- Oberon-numeric-case-statement/Sources/FPGAOberon2013/ORP.Mod	2025-01-05 17:34:05
+++ Oberon-type-case-statement-without-loopholes/Sources/FPGAOberon2013/ORP.Mod	2025-01-05 14:51:21
@@ -89,6 +89,11 @@
     END
   END CheckExport;
 
+  PROCEDURE CheckCase(VAR x: ORG.Item);
+  BEGIN (*x.obj is a simple identifier of pointer type in a type case statement that is not followed by a selector*)
+    IF (x.obj # NIL) & (x.obj.lev > 0) & (x.obj.exno > 0) & (x.obj.type.form = ORB.Pointer) THEN x.rdo := TRUE END
+  END CheckCase;
+
   PROCEDURE IsExtension(t0, t1: ORB.Type): BOOLEAN;
   BEGIN (*t1 is an extension of t0*)
     RETURN (t0 = t1) OR (t1 # NIL) & IsExtension(t0, t1.base)
@@ -318,7 +323,7 @@
     IF sym = ORS.ident THEN
       qualident(obj);  
       IF obj.class = ORB.SFunc THEN StandFunc(x, obj.val, obj.type)
-      ELSE ORG.MakeItem(x, obj, level); selector(x);
+      ELSE ORG.MakeItem(x, obj, level); selector(x); CheckCase(x);
         IF sym = ORS.lparen THEN
           ORS.Get(sym);
           IF (x.type.form = ORB.Proc) & (x.type.base.form # ORB.NoTyp) THEN
@@ -484,12 +489,12 @@
 
     PROCEDURE TypeCasePart(obj: ORB.Object);
       VAR L0: LONGINT;
-    BEGIN Check(ORS.of, "OF expected"); L0 := 0;
+    BEGIN Check(ORS.of, "OF expected"); L0 := 0; INC(obj.exno);
       WHILE (sym < ORS.end) OR (sym = ORS.bar) DO
         IF sym = ORS.bar THEN ORS.Get(sym) ELSE TypeCase(obj, L0) END
       END ;
       IF sym = ORS.else THEN ORS.Get(sym); StatSequence END ;
-      ORG.FixLink(L0)
+      ORG.FixLink(L0); DEC(obj.exno)
     END TypeCasePart;
 
     PROCEDURE CaseLabel(VAR x: ORG.Item);
@@ -556,7 +561,7 @@
         IF x.mode = ORB.SProc THEN StandProc(obj.val)
         ELSE selector(x);
           IF sym = ORS.becomes THEN (*assignment*)
-            ORS.Get(sym); CheckReadOnly(x); expression(y);
+            ORS.Get(sym); CheckCase(x); CheckReadOnly(x); expression(y);
             IF CompTypes(x.type, y.type, FALSE) THEN
               IF (x.type.form <= ORB.Pointer) OR (x.type.form = ORB.Proc) THEN ORG.Store(x, y)
               ELSE ORG.StoreStruct(x, y)
@@ -630,8 +635,8 @@
       ELSIF sym = ORS.case THEN
         ORS.Get(sym); x.obj := NIL; expression(x);
         IF x.type.form IN {ORB.Int, ORB.Byte, ORB.Char} THEN NumericCasePart(x)
-        ELSIF (x.obj # NIL) &
-          ((x.type.form = ORB.Pointer) & (x.type.base.form = ORB.Record) OR
+        ELSIF (x.obj # NIL) & (x.obj.lev > 0) &
+          ((x.type.form = ORB.Pointer) & (x.mode = ORB.Var) & (x.type.base.form = ORB.Record) OR
            (x.type.form = ORB.Record) & (x.mode = ORB.Par)) THEN TypeCasePart(x.obj)
         ELSE ORS.Mark("invalid case variable"); SkipCase
         END ;
```

-------------------------------------------------------------------------------------

**DIFFERENCES IN EXTENDED OBERON**

Differences to the repository **http://github.com/andreaspirklbauer/Oberon-extended**:

**ORP**

```diff
--- Oberon-extended/Sources/ORP.Mod	2025-01-05 17:24:30
+++ Oberon-type-case-statement-without-loopholes/Sources/ExtendedOberon/ORP.Mod	2025-01-05 17:37:49
@@ -91,6 +91,11 @@
     END
   END CheckExport;
 
+  PROCEDURE CheckCase(VAR x: ORG.Item);
+  BEGIN (*x.obj is a simple identifier of pointer type in a type case statement that is not followed by a selector*)
+    IF (x.obj # NIL) & (x.obj.lev > 0) & (x.obj.exno > 0) & (x.obj.type.form = ORB.Pointer) THEN x.rdo := TRUE END
+  END CheckCase;
+
   PROCEDURE IsExtension(t0, t1: ORB.Type): BOOLEAN;
   BEGIN (*t1 is an extension of t0*)
     RETURN (t0 = t1) OR (t1 # NIL) & IsExtension(t0, t1.base)
@@ -354,7 +359,7 @@
     IF sym = ORS.ident THEN
       qualident(obj);
       IF obj.class = ORB.SFunc THEN StandFunc(x, obj.val, obj.type)
-      ELSE ORG.MakeItem(x, obj); selector(x);
+      ELSE ORG.MakeItem(x, obj); selector(x); CheckCase(x);
         IF sym = ORS.lparen THEN
           ORS.Get(sym);
           IF (x.type.form IN {ORB.Proc, ORB.TProc}) & (x.type.base.form # ORB.NoTyp) THEN
@@ -530,12 +535,12 @@
 
     PROCEDURE TypeCasePart(obj: ORB.Object);
       VAR L0: LONGINT;
-    BEGIN Check(ORS.of, "OF expected"); L0 := 0;
+    BEGIN Check(ORS.of, "OF expected"); L0 := 0; INC(obj.exno);
       WHILE sym <= ORS.bar DO
         IF sym = ORS.bar THEN ORS.Get(sym) ELSE TypeCase(obj, L0) END
       END ;
       IF sym = ORS.else THEN ORS.Get(sym); StatSequence END ;
-      ORG.FixLink(L0)
+      ORG.FixLink(L0); DEC(obj.exno)
     END TypeCasePart;
 
     PROCEDURE CaseLabel(VAR x: ORG.Item);
@@ -603,7 +608,7 @@
         IF x.mode = ORB.SProc THEN StandProc(obj.val)
         ELSE selector(x);
           IF sym = ORS.becomes THEN (*assignment*)
-            ORS.Get(sym); CheckReadOnly(x); expression(y);
+            ORS.Get(sym); CheckCase(x); CheckReadOnly(x); expression(y);
             IF CompTypes(x.type, y.type, FALSE) THEN
               IF (x.type.form <= ORB.Pointer) OR (x.type.form = ORB.Proc) THEN ORG.Store(x, y)
               ELSE ORG.StoreStruct(x, y)
@@ -677,8 +682,8 @@
       ELSIF sym = ORS.case THEN
         ORS.Get(sym); x.obj := NIL; expression(x);
         IF x.type.form IN {ORB.Int, ORB.Byte, ORB.Char} THEN NumericCasePart(x)
-        ELSIF (x.obj # NIL) &
-          ((x.type.form = ORB.Pointer) & (x.type.base.form = ORB.Record) OR
+        ELSIF (x.obj # NIL) & (x.obj.lev > 0) &
+          ((x.type.form = ORB.Pointer) & (x.mode = ORB.Var) & (x.type.base.form = ORB.Record) OR
            (x.type.form = ORB.Record) & (x.mode = ORB.Par)) THEN TypeCasePart(x.obj)
         ELSE ORS.Mark("invalid case variable"); SkipCase
         END ;
```
