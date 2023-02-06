
--  Based on inspirel_ada-python_demo

with Interfaces.C;

with Ada.Assertions; use Ada.Assertions;
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Unchecked_Conversion;

--  with Basic_Printing; use Basic_Printing;
with Python_API; use Python_API;
with Tuple_Builder; use Tuple_Builder;

package body Python is

   --  -------------------------------------------------------------------------

   procedure Initialize (Program_Name : String := "") is
      use  Interfaces.C;
   begin
      if Program_Name /= "" then
         declare
            C_Name           : constant Interfaces.C.char_array :=
                                 To_C (Program_Name);
            Program_Name_Ptr : constant access Interfaces.C.char_array :=
                                 new char_array'(C_Name);
         begin
            Py_SetProgramName (Program_Name_Ptr.all);
         end;
      end if;

      Py_Initialize;

      Execute_String ("import sys");
      Execute_String ("import os");
      Execute_String ("sys.path.append('.')");
      Execute_String ("sys.path.append('./src')");
      Execute_String ("sys.path.append('../..')");
      Execute_String ("sys.path.append('../../python')");
      Execute_String ("sys.path.append('../../tree')");

   end Initialize;

   --  -------------------------------------------------------------------------

   procedure Finalize is
   begin
      Py_Finalize;
   end Finalize;

   --  -------------------------------------------------------------------------

   procedure Execute_String (Script : String) is
      use Interfaces.C;
      Dummy : int;
   begin
      Dummy := PyRun_SimpleString (Interfaces.C.To_C (Script));
      if Dummy /= 0 then
         Put_Line ("Python.Execute_String caused a Python exception!");
      end if;

   end Execute_String;

   --  -------------------------------------------------------------------------

   function Import_File (File_Name : String) return Module is
      use type System.Address;
      Routine_Name : constant String := "Python.Import_File ";
      PyFileName   : constant PyObject :=
                       PyString_FromString (Interfaces.C.To_C (File_Name));
   begin
      Execute_String ("cwd = os.getcwd()");
      Execute_String ("os.path.join (cwd, '/src/py_package')");
      Execute_String ("os.chdir(cwd + '/src/py_package')");

      declare
         M  : constant PyObject := PyImport_Import (PyFileName); 
      begin
         Py_DecRef (PyFileName);
         if M = System.Null_Address then
            Put ("PyErr: ");
            PyErr_Print;
            raise Interpreter_Error with Routine_Name &
              "cannot load module from file " & File_Name &
              " or " & File_Name & " is not a module";
         end if;

         return Module (M);
      end;

   end Import_File;

   --  -------------------------------------------------------------------------

   procedure Close_Module (M : Module) is
   begin
      Py_DecRef (PyObject (M));
   end Close_Module;

   --  -------------------------------------------------------------------------
   --  helpers for use from all overloaded Call subprograms
   function Get_Symbol (M : in Module; Function_Name : in String)
                        return PyObject is
      use type System.Address;
      Routine_Name : constant String := "Python.Get_Symbol ";
      PyModule     : constant PyObject := PyObject (M);
      --  PyObject_GetAttrString retrieves the attribute named Function_Name
      --  from the object PyModule.
      F            : constant PyObject :=
                       PyObject_GetAttrString
                         (PyModule, Interfaces.C.To_C (Function_Name));
   begin
      if F = System.Null_Address then
         Put_Line (Routine_Name & "Python error message:");
         PyErr_Print;
         raise Interpreter_Error with "Cannot find function " & Function_Name;
      end if;

      return F;

   end Get_Symbol;

   --  -------------------------------------------------------------------------

   function Call_Object (PyFunc, PyParams : PyObject) return PyObject is
      use Interfaces.C;
      use type System.Address;
      Routine_Name : constant String := "Python.Call_Object ";
      PyResult     : PyObject;
   begin
      Assert (PyFunc /= System.Null_Address, Routine_Name & "PyFunc is null.");  
      Assert (PyCallable_Check (PyFunc) /= 0, Routine_Name &
                "PyCallable_Check is null."); 
      Assert (PyParams /= System.Null_Address, Routine_Name &
                "PyParams is null.");
      PyResult := PyObject_CallObject (PyFunc, PyParams);

      if PyResult = System.Null_Address then
         New_Line;
         Put_Line (Routine_Name & "Python error message:");
         PyErr_Print;
         raise Interpreter_Error with Routine_Name & "failed.";
      end if;

      return PyResult;

   exception
      when E : others =>
         raise Interpreter_Error with Routine_Name & "exception: " &
           Exception_Message (E);

   end Call_Object;     

   --  -------------------------------------------------------------------------

   procedure Call (M : Module; Function_Name : String) is
      Func     : PyObject;
      PyResult : PyObject;
   begin
      Func := Get_Symbol (M, Function_Name);
      PyResult := PyObject_CallObject (Func, System.Null_Address);
      
      Py_DecRef (Func);
      Py_DecRef (PyResult);

   end Call;

   --  -------------------------------------------------------------------------

   function Call (M : Module; Function_Name : String)
                  return Python_API.PyObject is
      Func     : PyObject;
      PyResult : PyObject;
   begin
      Func := Get_Symbol (M, Function_Name);
      PyResult := PyObject_CallObject (Func, System.Null_Address);
      
      Py_DecRef (Func);
      
      return PyResult;

   end Call;

   --  -------------------------------------------------------------------------

   procedure Call (M : Module; Function_Name, A : String) is

      function Py_BuildValue (Format : Interfaces.C.char_array;
                              A      : Interfaces.C.char_array) return PyObject;
      pragma Import (C, Py_BuildValue, "Py_BuildValue");

      F        : constant PyObject := Get_Symbol (M, Function_Name);
      PyParams : PyObject;
      PyResult : PyObject;
      Result   : aliased Interfaces.C.long;
   begin
      PyParams := Py_BuildValue (Interfaces.C.To_C ("(s)"),
                                 Interfaces.C.To_C (A));
      PyResult := Call_Object (F, PyParams);
      Result := PyInt_AsLong (PyResult);
      
      Py_DecRef (F);
      Py_DecRef (PyParams);
      Py_DecRef (PyResult);

   end Call;

   -- --------------------------------------------------------------------------

   function Call (M : Module; Function_Name : String; A : Integer)
                  return Integer is
      use Interfaces.C;

      function Py_BuildValue (Format : char_array; A : int) return PyObject;
      pragma Import (C, Py_BuildValue, "Py_BuildValue");
      F        : constant PyObject := Get_Symbol (M, Function_Name);
      PyParams : PyObject;
      PyResult : PyObject;
      Result   : aliased long;
   begin
      PyParams := Py_BuildValue (To_C ("(i)"), int (A));
      PyResult := Call_Object (F, PyParams);
      Result := PyInt_AsLong (PyResult);
      
      Py_DecRef (F);
      Py_DecRef (PyParams);
      Py_DecRef (PyResult);

      return Integer (Result);

   end Call;

   -- --------------------------------------------------------------------------
 
   function Call (M : Module; Function_Name : String; A : Integer)
                  return Python_API.PyObject is
      use Interfaces.C;

      function Py_BuildValue (Format : char_array; A : int) return PyObject;
      pragma Import (C, Py_BuildValue, "Py_BuildValue");
      F        : constant PyObject := Get_Symbol (M, Function_Name);
      PyParams : PyObject;
      PyResult : PyObject;
   begin
      PyParams := Py_BuildValue (To_C ("(i)"), int (A));
      PyResult := Call_Object (F, PyParams);
      
      Py_DecRef (F);
      Py_DecRef (PyParams);

      return PyResult;

   end Call;

   -- --------------------------------------------------------------------------

   function Call (M : Module; Function_Name : String; A, B : Integer)
                  return Integer is

      function Py_BuildValue (Format : Interfaces.C.char_array;
                              A      : Interfaces.C.int;
                              B      : Interfaces.C.int) return PyObject;
      pragma Import (C, Py_BuildValue, "Py_BuildValue");

      F        : constant PyObject := Get_Symbol (M, Function_Name);
      PyParams : PyObject;
      PyResult : PyObject;
      Result   : aliased Interfaces.C.long;
   begin
      PyParams := Py_BuildValue (Interfaces.C.To_C ("ii"), Interfaces.C.int (A),
                                 Interfaces.C.int (B));
      PyResult := Call_Object (F, PyParams);
      Result := PyInt_AsLong (PyResult);
      
      Py_DecRef (F);
      Py_DecRef (PyParams);
      Py_DecRef (PyResult);

      return Integer (Result);
   end Call;

   --  -------------------------------------------------------------------------

   function Call (M : Module; Function_Name : String; A : Integer; B : Float)
                  return Python_API.PyObject is

      function Py_BuildValue (Format : Interfaces.C.char_array;
                              A      : Interfaces.C.int;
                              B      : Interfaces.C.double) return PyObject;
      pragma Import (C, Py_BuildValue, "Py_BuildValue");

      F        : constant PyObject := Get_Symbol (M, Function_Name);
      PyParams : PyObject;
      PyResult : PyObject;
   begin
      PyParams := Py_BuildValue (Interfaces.C.To_C ("id"), Interfaces.C.int (A),
                                 Interfaces.C.double (B));
      PyResult := Call_Object (F, PyParams);
      
      Py_DecRef (F);
      Py_DecRef (PyParams);

      return PyResult;
      
   end Call;

   --  -------------------------------------------------------------------------

   procedure Call (M : Module; Function_Name : String;
                   A : ML_Arrays_And_Matrices.Integer_Matrix) is

      function Py_BuildValue (Format  : Interfaces.C.char_array;
                              T1      : PyObject) return PyObject;
      pragma Import (C, Py_BuildValue, "Py_BuildValue");

      F        : constant PyObject := Get_Symbol (M, Function_Name);
      A_Tuple  : constant PyObject := To_Tuple (A);
      PyParams : PyObject;
      PyResult : PyObject;
      Result   : aliased Interfaces.C.long;
   begin
      PyParams :=
        Py_BuildValue (Interfaces.C.To_C ("O"), A_Tuple);

      PyResult := Call_Object (F, PyParams);
      Result := PyInt_AsLong (PyResult);

      Py_DecRef (F);
      Py_DecRef (A_Tuple);
      Py_DecRef (PyParams);
      Py_DecRef (PyResult);

   end Call;

   --  -------------------------------------------------------------------------

   procedure Call (M    : Module; Function_Name : String;
                   A, B : ML_Arrays_And_Matrices.Integer_Matrix) is

      function Py_BuildValue (Format  : Interfaces.C.char_array;
                              T1, T2  : PyObject) return PyObject;
      pragma Import (C, Py_BuildValue, "Py_BuildValue");

      F        : constant PyObject := Get_Symbol (M, Function_Name);
      A_Tuple  : constant PyObject := To_Tuple (A);
      B_Tuple  : constant PyObject := To_Tuple (B);
      PyParams : PyObject;
      PyResult : PyObject;
   begin
      PyParams :=
        Py_BuildValue (Interfaces.C.To_C ("OO"), A_Tuple, B_Tuple);

      PyResult := Call_Object (F, PyParams);

      Py_DecRef (F);
      Py_DecRef (A_Tuple);
      Py_DecRef (B_Tuple);
      Py_DecRef (PyParams);
      Py_DecRef (PyResult);

   end Call;

   --  -------------------------------------------------------------------------

   procedure Call (M    : Module; Function_Name : String;
                   A, B : ML_Arrays_And_Matrices.Real_Float_Matrix) is

      function Py_BuildValue (Format  : Interfaces.C.char_array;
                              T1, T2  : PyObject) return PyObject;
      pragma Import (C, Py_BuildValue, "Py_BuildValue");

      F        : constant PyObject := Get_Symbol (M, Function_Name);
      A_Tuple  : constant PyObject := To_Tuple (A);
      B_Tuple  : constant PyObject := To_Tuple (B);
      PyParams : PyObject;
      PyResult : PyObject;
   begin
      PyParams :=
        Py_BuildValue (Interfaces.C.To_C ("OO"), A_Tuple, B_Tuple);

      PyResult := Call_Object (F, PyParams);

      Py_DecRef (F);
      Py_DecRef (A_Tuple);
      Py_DecRef (B_Tuple);
      Py_DecRef (PyParams);
      Py_DecRef (PyResult);

   end Call;

   --  -------------------------------------------------------------------------

   procedure Call (M    : Module; Function_Name : String;
                   A, B : ML_Arrays_And_Matrices.Integer_Matrix;
                   C    : ML_Types.Unbounded_List) is

      function Py_BuildValue (Format      : Interfaces.C.char_array;
                              T1, T2, T3  : PyObject) return PyObject;
      pragma Import (C, Py_BuildValue, "Py_BuildValue");

      --        Routine_Name : constant String := "Python.Call ABC 2 int + UBL ";
      F        : constant PyObject := Get_Symbol (M, Function_Name);
      A_Tuple  : constant PyObject := To_Tuple (A);
      B_Tuple  : constant PyObject := To_Tuple (B);
      C_Tuple  : constant PyObject := To_Tuple (C);
      PyParams : PyObject;
      PyResult : PyObject;
      Result   : aliased Interfaces.C.long;
   begin
      PyParams :=
        Py_BuildValue (Interfaces.C.To_C ("OOO"), A_Tuple, B_Tuple, C_Tuple);
      PyResult := Call_Object (F, PyParams);
      Result := PyInt_AsLong (PyResult);

      Py_DecRef (F);
      Py_DecRef (A_Tuple);
      Py_DecRef (B_Tuple);
      Py_DecRef (C_Tuple);
      Py_DecRef (PyParams);
      Py_DecRef (PyResult);

   end Call;

   --  -------------------------------------------------------------------------

   procedure Call (M : Module; Function_Name : String;  A, B, C, D : Float) is
      use Interfaces.C;
      function Py_BuildValue (Format             : Interfaces.C.char_array;
                              A_C, B_C, C_C, D_C : double) return PyObject;
      pragma Import (C, Py_BuildValue, "Py_BuildValue");

      F        : constant PyObject := Get_Symbol (M, Function_Name);
      A_C      : constant double := double (A);
      B_C      : constant double := double (B);
      C_C      : constant double := double (C);
      D_C      : constant double := double (D);
      PyParams : PyObject;
      PyResult : PyObject;
      Result   : aliased Interfaces.C.long;
   begin
      PyParams :=
        Py_BuildValue (Interfaces.C.To_C ("dddd"), A_C, B_C, C_C, D_C);
      PyResult := Call_Object (F, PyParams);
      Result := PyInt_AsLong (PyResult);

      Py_DecRef (F);
      Py_DecRef (PyParams);
      Py_DecRef (PyResult);

   end Call;

   --  -------------------------------------------------------------------------

   procedure Call (M : Module; Function_Name : String; A, B, C, D : Float;
                   E : String) is
      use Interfaces.C;
      function Py_BuildValue 
        (Format             : char_array;
         A_C, B_C, C_C, D_C : double; E_C : char_array) return PyObject;
      pragma Import (C, Py_BuildValue, "Py_BuildValue");

      F        : constant PyObject := Get_Symbol (M, Function_Name);
      A_C      : constant double := double (A);
      B_C      : constant double := double (B);
      C_C      : constant double := double (C);
      D_C      : constant double := double (D);
      E_C      : constant char_array := To_C (E);
      PyParams : PyObject;
      PyResult : PyObject;
      Result   : aliased Interfaces.C.long;
   begin
      PyParams :=
        Py_BuildValue (Interfaces.C.To_C ("dddds"), A_C, B_C, C_C, D_C, E_C);
      PyResult := Call_Object (F, PyParams);
      Result := PyInt_AsLong (PyResult);

      Py_DecRef (F);
      Py_DecRef (PyParams);
      Py_DecRef (PyResult);

   end Call;

   --  -------------------------------------------------------------------------

   procedure Call (M : Module; Function_Name : String;
                   A : ML_Arrays_And_Matrices.Real_Float_Matrix) is

      function Py_BuildValue (Format : Interfaces.C.char_array;
                              T1     : PyObject) return PyObject;
      pragma Import (C, Py_BuildValue, "Py_BuildValue");

      F        : constant PyObject := Get_Symbol (M, Function_Name);
      A_Tuple  : constant PyObject := To_Tuple (A);
      PyParams : constant PyObject := 
                   Py_BuildValue (Interfaces.C.To_C ("(O)"), A_Tuple);
      PyResult : PyObject;
      Result   : aliased Interfaces.C.long;
   begin
      PyResult := Call_Object (F, PyParams);
      Result := PyInt_AsLong (PyResult);

      Py_DecRef (F);
      Py_DecRef (A_Tuple);
      Py_DecRef (PyParams);
      Py_DecRef (PyResult);

   end Call;

   --  -------------------------------------------------------------------------

   procedure Call (M    : Module; Function_Name : String;
                   A    : ML_Arrays_And_Matrices.Real_Float_Matrix;
                   B    : ML_Arrays_And_Matrices.Integer_Array) is

      function Py_BuildValue (Format : Interfaces.C.char_array;
                              T1, T2 : PyObject) return PyObject;
      pragma Import (C, Py_BuildValue, "Py_BuildValue");

      F        : constant PyObject := Get_Symbol (M, Function_Name);
      A_Tuple  : constant PyObject := To_Tuple (A);
      B_Tuple  : constant PyObject := To_Tuple (B);
      PyParams : constant PyObject := 
                   Py_BuildValue (Interfaces.C.To_C ("OO"), A_Tuple, B_Tuple);
      PyResult : PyObject;
      Result   : aliased Interfaces.C.long;
   begin
      PyResult := Call_Object (F, PyParams);
      Result := PyInt_AsLong (PyResult);

      Py_DecRef (F);
      Py_DecRef (A_Tuple);
      Py_DecRef (B_Tuple);
      Py_DecRef (PyParams);
      Py_DecRef (PyResult);

   end Call;

   --  -------------------------------------------------------------------------

   procedure Call (M    : Module; Function_Name : String;
                   A, B : ML_Arrays_And_Matrices.Real_Float_Vector) is

      function Py_BuildValue (Format  : Interfaces.C.char_array;
                              T1, T2  : PyObject) return PyObject;
      pragma Import (C, Py_BuildValue, "Py_BuildValue");

      F        : constant PyObject := Get_Symbol (M, Function_Name);
      A_Tuple  : constant PyObject := To_Tuple (A);
      B_Tuple  : constant PyObject := To_Tuple (B);
      PyParams : PyObject;
      PyResult : PyObject;
   begin
      PyParams :=
        Py_BuildValue (Interfaces.C.To_C ("OO"), A_Tuple, B_Tuple);

      PyResult := Call_Object (F, PyParams);

      Py_DecRef (F);
      Py_DecRef (A_Tuple);
      Py_DecRef (B_Tuple);
      Py_DecRef (PyParams);
      Py_DecRef (PyResult);

   end Call;

   --  -------------------------------------------------------------------------

   procedure Call (M    : Module; Function_Name : String;
                   A    : ML_Arrays_And_Matrices.Real_Float_Matrix;
                   B    : ML_Arrays_And_Matrices.Integer_Array;
                   C    : ML_Arrays_And_Matrices.Real_Float_Matrix;
                   D    : ML_Arrays_And_Matrices.Integer_Array) is
      --        Routine_Name : constant String := "Python.Call ABCD ";

      function Py_BuildValue (Format          : Interfaces.C.char_array;
                              T1, T2, T3, T4  : PyObject) return PyObject;
      pragma Import (C, Py_BuildValue, "Py_BuildValue");

      Py_Func  : PyObject;
      A_Tuple  : constant PyObject := To_Tuple (A);
      B_Tuple  : constant PyObject := To_Tuple (B);
      C_Tuple  : constant PyObject := To_Tuple (C);
      D_Tuple  : constant PyObject := To_Tuple (D);
      PyParams : PyObject;
      PyResult : PyObject;
      Result   : aliased Interfaces.C.long;
   begin
      Py_Func := Get_Symbol (M, Function_Name);
      PyParams :=
        Py_BuildValue (Interfaces.C.To_C ("OOOO"),
                       A_Tuple, B_Tuple, C_Tuple, D_Tuple);
      Py_DecRef (A_Tuple);
      Py_DecRef (B_Tuple);
      Py_DecRef (C_Tuple);
      Py_DecRef (D_Tuple);

      PyResult := Call_Object (Py_Func, PyParams);
      Py_DecRef (Py_Func);
      Py_DecRef (PyParams);

      Result := PyInt_AsLong (PyResult);     
      Py_DecRef (PyResult);

   end Call;

   --  -------------------------------------------------------------------------

   procedure Call (M : Module; Function_Name : String;
                   A : in out ML_Arrays_And_Matrices.Real_Float_Matrix;
                   B : in out ML_Arrays_And_Matrices.Integer_Matrix;
                   C : in out ML_Arrays_And_Matrices.Real_Float_Matrix;
                   D : in out ML_Arrays_And_Matrices.Integer_Matrix) is
      use Interfaces.C;
      use ML_Arrays_And_Matrices; 
      Routine_Name : constant String := "Python.Call ABCD out ";

      procedure Parse_Tuple (Tuple : PyObject; M : in out Real_Float_Matrix) is
         T_Row : PyObject;
         T_Col : PyObject;
      begin
         Assert (M'Length = integer ( PyTuple_Size (Tuple) ), Routine_Name &
                   "Parse_Tuple Tuple Size" & int'Image (PyTuple_Size (Tuple))
                 & " /= M'Length" & Integer'Image (M'Length));
         for row in 1 .. PyTuple_Size (Tuple) loop
            T_Row := PyTuple_GetItem (Tuple, row - 1);
            for col in 1 .. PyTuple_Size (T_Row) loop
               T_Col := PyTuple_GetItem (T_Row, col - 1);
               M (Integer (row), Integer (col)) :=
                 Float (PyFloat_AsDouble (PyTuple_GetItem (T_Col, 0)));
            end loop;
         end loop;
      end Parse_Tuple;
      
      procedure Parse_Tuple (Tuple : PyObject; M : in out Integer_Matrix) is
         T_Row : PyObject;
         T_Col : PyObject;
      begin
         Assert (M'Length = integer ( PyTuple_Size (Tuple) ), Routine_Name &
                   "Parse_Tuple Tuple Size" & int'Image (PyTuple_Size (Tuple))
                 & " /= M'Length" & Integer'Image (M'Length));
         for row in 1 .. PyTuple_Size (Tuple) loop
            T_Row := PyTuple_GetItem (Tuple, row - 1);
            for col in 1 .. PyTuple_Size (T_Row) loop
               T_Col := PyTuple_GetItem (T_Row, col - 1);
               M (Integer (row), Integer (col)) :=
                 Integer (PyLong_AsLong (PyTuple_GetItem (T_Col, 0)));
            end loop;
         end loop;
      end Parse_Tuple;
      
      function Py_BuildValue (Format : char_array;
                              T1, T2 : int) return PyObject;
      pragma Import (C, Py_BuildValue, "Py_BuildValue");

      Py_Func  : PyObject;
      A_Length : constant int := int (A'Length);
      C_Length : constant int := int (C'Length);
      A_Tuple  : PyObject;
      B_Tuple  : PyObject;
      C_Tuple  : PyObject;
      D_Tuple  : PyObject;
      PyParams : PyObject;
      PyResult : PyObject;
      Result   : aliased int;
   begin
      Py_Func := Get_Symbol (M, Function_Name);
      PyParams := Py_BuildValue (To_C ("ii"), A_Length, C_Length);

      PyResult := Call_Object (Py_Func, PyParams);
      Py_DecRef (Py_Func);
      Py_DecRef (PyParams);

      Result := PyTuple_Size (PyResult);
      A_Tuple := PyTuple_GetItem (PyResult, 0);
      B_Tuple := PyTuple_GetItem (PyResult, 1);
      C_Tuple := PyTuple_GetItem (PyResult, 2);
      D_Tuple := PyTuple_GetItem (PyResult, 3);
      
      Parse_Tuple (A_Tuple, A);
      Parse_Tuple (B_Tuple, B);
      Parse_Tuple (C_Tuple, C);
      Parse_Tuple (D_Tuple, D);
      
      Py_DecRef (PyResult);
      Py_DecRef (A_Tuple);
      Py_DecRef (B_Tuple);
      Py_DecRef (C_Tuple);
      Py_DecRef (D_Tuple);

   end Call;

   --  -------------------------------------------------------------------------

   procedure Call (M    : Module; Function_Name : String;
                   A    : ML_Arrays_And_Matrices.Real_Float_Matrix;
                   B    : ML_Arrays_And_Matrices.Real_Float_Vector;
                   C    : ML_Arrays_And_Matrices.Real_Float_Matrix;
                   D    : ML_Arrays_And_Matrices.Real_Float_Vector) is
      --        Routine_Name : constant String := "Python.Call ABCD MV ";

      function Py_BuildValue (Format          : Interfaces.C.char_array;
                              T1, T2, T3, T4  : PyObject) return PyObject;
      pragma Import (C, Py_BuildValue, "Py_BuildValue");

      Py_Func  : PyObject;
      A_Tuple  : constant PyObject := To_Tuple (A);
      B_Tuple  : constant PyObject := To_Tuple (B);
      C_Tuple  : constant PyObject := To_Tuple (C);
      D_Tuple  : constant PyObject := To_Tuple (D);
      PyParams : PyObject;
      PyResult : PyObject;
   begin
      Py_Func := Get_Symbol (M, Function_Name);
      PyParams :=
        Py_BuildValue (Interfaces.C.To_C ("OOOO"),
                       A_Tuple, B_Tuple, C_Tuple, D_Tuple);

      PyResult := Call_Object (Py_Func, PyParams);
      Py_DecRef (Py_Func);
      Py_DecRef (A_Tuple);
      Py_DecRef (B_Tuple);
      Py_DecRef (C_Tuple);
      Py_DecRef (D_Tuple);
      Py_DecRef (PyParams);
      Py_DecRef (PyResult);

   end Call;

   --  -------------------------------------------------------------------------

   procedure Call (M    : Module; Function_Name : String;
                   A    : ML_Arrays_And_Matrices.Real_Float_Matrix;
                   B    : ML_Arrays_And_Matrices.Integer_Matrix) is

      function Py_BuildValue (Format  : Interfaces.C.char_array;
                              T1, T2  : PyObject) return PyObject;
      pragma Import (C, Py_BuildValue, "Py_BuildValue");

      F        : constant PyObject := Get_Symbol (M, Function_Name);
      A_Tuple  : constant PyObject := To_Tuple (A);
      B_Tuple  : constant PyObject := To_Tuple (B);
      PyParams : PyObject;
      PyResult : PyObject;
      Result   : aliased Interfaces.C.long;
   begin
      PyParams :=
        Py_BuildValue (Interfaces.C.To_C ("OO"), A_Tuple, B_Tuple);
      PyResult := Call_Object (F, PyParams);
      Result := PyInt_AsLong (PyResult);

      Py_DecRef (F);
      Py_DecRef (A_Tuple);
      Py_DecRef (B_Tuple);
      Py_DecRef (PyParams);
      Py_DecRef (PyResult);

   end Call;

   --  -------------------------------------------------------------------------

   procedure Call (M    : Module; Function_Name : String;
                   A    : ML_Arrays_And_Matrices.Real_Float_Matrix;
                   B    : ML_Arrays_And_Matrices.Real_Float_Vector) is

      function Py_BuildValue (Format  : Interfaces.C.char_array;
                              T1, T2  : PyObject) return PyObject;
      pragma Import (C, Py_BuildValue, "Py_BuildValue");

      F        : constant PyObject := Get_Symbol (M, Function_Name);
      A_Tuple  : constant PyObject := To_Tuple (A);
      B_Tuple  : constant PyObject := To_Tuple (B);
      PyParams : PyObject;
      PyResult : PyObject;
      Result   : aliased Interfaces.C.long;
   begin
      PyParams :=
        Py_BuildValue (Interfaces.C.To_C ("OO"), A_Tuple, B_Tuple);
      PyResult := Call_Object (F, PyParams);
      Result := PyInt_AsLong (PyResult);

      Py_DecRef (F);
      Py_DecRef (A_Tuple);
      Py_DecRef (B_Tuple);
      Py_DecRef (PyParams);
      Py_DecRef (PyResult);

   end Call;

   --  -------------------------------------------------------------------------

   procedure Call (M : Module; Function_Name : String;
                   A : ML_Arrays_And_Matrices.Real_Float_Vector) is

      function Py_BuildValue (Format : Interfaces.C.char_array;
                              T1     : PyObject) return PyObject;
      pragma Import (C, Py_BuildValue, "Py_BuildValue");

      F        : constant PyObject := Get_Symbol (M, Function_Name);
      A_Tuple  : constant PyObject := To_Tuple (A);
      PyParams : PyObject;
      PyResult : PyObject;
      Result   : aliased Interfaces.C.long;
   begin
      PyParams :=
        Py_BuildValue (Interfaces.C.To_C ("(O)"), A_Tuple);
      PyResult := Call_Object (F, PyParams);
      Result := PyInt_AsLong (PyResult);

      Py_DecRef (F);
      Py_DecRef (A_Tuple);
      Py_DecRef (PyParams);
      Py_DecRef (PyResult);

   end Call;

   --  -------------------------------------------------------------------------

   procedure Call (M       : Module; Function_Name : String;
                   A, B, C : ML_Arrays_And_Matrices.Real_Float_Vector) is

      function Py_BuildValue (Format     : Interfaces.C.char_array;
                              T1, T2, T3 : PyObject) return PyObject;
      pragma Import (C, Py_BuildValue, "Py_BuildValue");

      F        : constant PyObject := Get_Symbol (M, Function_Name);
      A_Tuple  : constant PyObject := To_Tuple (A);
      B_Tuple  : constant PyObject := To_Tuple (B);
      C_Tuple  : constant PyObject := To_Tuple (C);
      PyParams : PyObject;
      PyResult : PyObject;
      Result   : aliased Interfaces.C.long;
   begin
      PyParams :=
        Py_BuildValue (Interfaces.C.To_C ("OOO"), A_Tuple, B_Tuple, C_Tuple);
      PyResult := Call_Object (F, PyParams);
      Result := PyInt_AsLong (PyResult);

      Py_DecRef (F);
      Py_DecRef (A_Tuple);
      Py_DecRef (B_Tuple);
      Py_DecRef (C_Tuple);
      Py_DecRef (PyParams);
      Py_DecRef (PyResult);

   end Call;

   --  -------------------------------------------------------------------------

   procedure Call (M    : Module; Function_Name : String;
                   A, B : ML_Arrays_And_Matrices.Real_Float_Vector;
                   C    : ML_Arrays_And_Matrices.Real_Float_Matrix) is

      function Py_BuildValue (Format     : Interfaces.C.char_array;
                              T1, T2, T3 : PyObject) return PyObject;
      pragma Import (C, Py_BuildValue, "Py_BuildValue");

      F        : constant PyObject := Get_Symbol (M, Function_Name);
      A_Tuple  : constant PyObject := To_Tuple (A);
      B_Tuple  : constant PyObject := To_Tuple (B);
      C_Tuple  : constant PyObject := To_Tuple (C);
      PyParams : PyObject;
      PyResult : PyObject;
      Result   : aliased Interfaces.C.long;
   begin
      PyParams :=
        Py_BuildValue (Interfaces.C.To_C ("OOO"), A_Tuple, B_Tuple, C_Tuple);
      PyResult := Call_Object (F, PyParams);
      Result := PyInt_AsLong (PyResult);

      Py_DecRef (F);
      Py_DecRef (A_Tuple);
      Py_DecRef (B_Tuple);
      Py_DecRef (C_Tuple);
      Py_DecRef (PyParams);
      Py_DecRef (PyResult);

   end Call;

   --  -------------------------------------------------------------------------

   procedure Call (M : Module; Function_Name : String;
                   A : ML_Arrays_And_Matrices.Unsigned_8_Array_3D) is
      use Interfaces.C;
      
      function Py_BuildValue (Format  : Interfaces.C.char_array;
                              T1      : PyObject;
                              I1      : int) return PyObject;
      pragma Import (C, Py_BuildValue, "Py_BuildValue");

      --        Routine_Name : constant String := "Python.Call U8_3D ";
      F            : PyObject;
      Row_Length   : constant int := int (A'Length (2));
      A_Tuple      : PyObject;
      PyParams     :  PyObject;
      PyResult     : PyObject;
      Result       : aliased Interfaces.C.long;
   begin
      F := Get_Symbol (M, Function_Name);
      A_Tuple := To_Tuple (A);
      PyParams := Py_BuildValue (Interfaces.C.To_C ("(Oi)"),
                                 A_Tuple, Row_Length);
      PyResult := Call_Object (F, PyParams);
      Result := PyInt_AsLong (PyResult);

      Py_DecRef (F);
      Py_DecRef (A_Tuple);
      Py_DecRef (PyParams);
      Py_DecRef (PyResult);

   end Call;

   --  -------------------------------------------------------------------------

   procedure Call (M : Module; Function_Name : String;
                   A : NL_Types.Boolean_List_2D;
                   B : ML_Types.Bounded_String_List) is
      use System;
      function Py_BuildValue (Format  : Interfaces.C.char_array;
                              T1, T2  : PyObject)  return PyObject;
      pragma Import (C, Py_BuildValue, "Py_BuildValue");

      Routine_Name : constant String := "Python.Call 2 ";
      PyFunc       : constant PyObject := Get_Symbol (M, Function_Name);
      A_Tuple      : constant PyObject := To_Tuple (A);
      B_Tuple      : constant PyObject := To_Tuple (B);
      PyParams     : PyObject;
      PyResult     : PyObject;
      Result       : aliased Interfaces.C.long;
   begin
      Assert (A_Tuple /= Null_Address, Routine_Name & "A_Tuple is null");
      Assert (B_Tuple /= Null_Address, Routine_Name &
                "Labels_Tuple is null");

      PyParams :=
        Py_BuildValue (Interfaces.C.To_C ("OO"), A_Tuple, B_Tuple);
      Assert (PyParams /= Null_Address, Routine_Name & "PyParams is null");

      PyResult := Call_Object (PyFunc, PyParams);
      if PyResult = System.Null_Address then
         Put (Routine_Name & "Py error message: ");
         PyErr_Print;
      end if;

      Result := PyInt_AsLong (PyResult);
      Put_Line (Routine_Name & "Result: " & Interfaces.C.long'Image (Result));

      Py_DecRef (PyFunc);
      Py_DecRef (A_Tuple);
      Py_DecRef (B_Tuple);
      Py_DecRef (PyParams);
      Py_DecRef (PyResult);

   end Call;

   --  -------------------------------------------------------------------------

   procedure Call (M : Module; Function_Name : String;
                   A : NL_Types.Boolean_List_2D;
                   B : NL_Types.Boolean_List;
                   C : NL_Types.Boolean_List_2D) is
      use System;
      function Py_BuildValue (Format      : Interfaces.C.char_array;
                              T1, T2, T3  : PyObject)  return PyObject;
      pragma Import (C, Py_BuildValue, "Py_BuildValue");

      Routine_Name : constant String := "Python.Call 2 ";
      PyFunc       : constant PyObject := Get_Symbol (M, Function_Name);
      A_Tuple      : constant PyObject := To_Tuple (A);
      B_Tuple      : constant PyObject := To_Tuple (B);
      C_Tuple      : constant PyObject := To_Tuple (C);
      PyParams     : PyObject;
      PyResult     : PyObject;
      Result       : aliased Interfaces.C.long;
   begin
      Assert (A_Tuple /= Null_Address, Routine_Name & "A_Tuple is null");
      Assert (B_Tuple /= Null_Address, Routine_Name & "B_Tuple is null");
      Assert (C_Tuple /= Null_Address, Routine_Name & "C_Tuple is null");

      PyParams :=
        Py_BuildValue (Interfaces.C.To_C ("OOO"), A_Tuple, B_Tuple,C_Tuple);
      Assert (PyParams /= Null_Address, Routine_Name & "PyParams is null");

      PyResult := Call_Object (PyFunc, PyParams);
      if PyResult = System.Null_Address then
         Put (Routine_Name & "Py error message: ");
         PyErr_Print;
      end if;

      Result := PyInt_AsLong (PyResult);
      Put_Line (Routine_Name & "Result: " & Interfaces.C.long'Image (Result));

      Py_DecRef (PyFunc);
      Py_DecRef (A_Tuple);
      Py_DecRef (B_Tuple);
      Py_DecRef (C_Tuple);
      Py_DecRef (PyParams);
      Py_DecRef (PyResult);

   end Call;

   --  -------------------------------------------------------------------------

   procedure Call (M : Module; Function_Name : String;
                   A : NL_Types.Boolean_List_2D; B : NL_Types.Boolean_List;
                   C : NL_Types.Boolean_List_2D; D : ML_Types.Unbounded_List) is
      use System;
      function Py_BuildValue (Format         : Interfaces.C.char_array;
                              T1, T2, T3, T4 : PyObject)  return PyObject;
      pragma Import (C, Py_BuildValue, "Py_BuildValue");

      Routine_Name : constant String := "Python.Call 4 ";
      PyFunc       : constant PyObject := Get_Symbol (M, Function_Name);
      A_Tuple      : constant PyObject := To_Tuple (A);
      B_Tuple      : constant PyObject := To_Tuple (B);
      C_Tuple      : constant PyObject := To_Tuple (C);
      D_Tuple      : constant PyObject := To_Tuple (D);
      PyParams     : PyObject;
      PyResult     : PyObject;
      Result       : aliased Interfaces.C.long;
   begin
      Assert (A_Tuple /= Null_Address, Routine_Name & "A_Tuple is null");
      Assert (B_Tuple /= Null_Address, Routine_Name & "B_Tuple is null");
      Assert (C_Tuple /= Null_Address, Routine_Name & "C_Tuple is null");
      Assert (D_Tuple /= Null_Address, Routine_Name & "D_Tuple is null");

      PyParams :=
        Py_BuildValue (Interfaces.C.To_C ("OOOO"),
                       A_Tuple, B_Tuple, C_Tuple, D_Tuple);
      Assert (PyParams /= Null_Address, Routine_Name & "PyParams is null");

      PyResult := Call_Object (PyFunc, PyParams);
      if PyResult = System.Null_Address then
         Put (Routine_Name & "Py error message: ");
         PyErr_Print;
      end if;

      Result := PyInt_AsLong (PyResult);

      Py_DecRef (PyFunc);
      Py_DecRef (A_Tuple);
      Py_DecRef (B_Tuple);
      Py_DecRef (C_Tuple);
      Py_DecRef (PyParams);
      Py_DecRef (PyResult);

   end Call;

   --  -------------------------------------------------------------------------

   procedure Call (M : Module; Function_Name : String;
                   A : ML_Arrays_And_Matrices.Integer_Array_List) is
      use System;
      function Py_BuildValue (Format : Interfaces.C.char_array;
                              T1     : PyObject)  return PyObject;
      pragma Import (C, Py_BuildValue, "Py_BuildValue");

      Routine_Name : constant String := "Python.Call Integer_Array_List ";
      PyFunc       : constant PyObject := Get_Symbol (M, Function_Name);
      A_Tuple      : constant PyObject := To_Tuple (A);
      PyParams     : PyObject;
      PyResult     : PyObject;
      Result       : aliased Interfaces.C.long;
   begin
      Assert (A_Tuple /= Null_Address, Routine_Name & "A_Tuple is null");

      PyParams :=
        Py_BuildValue (Interfaces.C.To_C ("(O)"), A_Tuple);
      Assert (PyParams /= Null_Address, Routine_Name & "PyParams is null");

      PyResult := Call_Object (PyFunc, PyParams);
      if PyResult = System.Null_Address then
         Put (Routine_Name & "Py error message: ");
         PyErr_Print;
      end if;

      Result := PyInt_AsLong (PyResult);
      Put_Line (Routine_Name & " Result: " & Interfaces.C.long'Image (Result));

      Py_DecRef (PyFunc);
      Py_DecRef (A_Tuple);
      Py_DecRef (PyParams);
      Py_DecRef (PyResult);

   end Call;

   --  -------------------------------------------------------------------------

   procedure Call (M : Module; Function_Name : String;
                   A : ML_Arrays_And_Matrices.Integer_Array_List;
                   B : ML_Arrays_And_Matrices.Integer_Array_List) is
      use System;
      function Py_BuildValue (Format  : Interfaces.C.char_array;
                              T1, T2  : PyObject)  return PyObject;
      pragma Import (C, Py_BuildValue, "Py_BuildValue");

      Routine_Name : constant String := "Python.Call 2 * Integer_Array_List ";
      PyFunc       : constant PyObject := Get_Symbol (M, Function_Name);
      A_Tuple      : constant PyObject := To_Tuple (A);
      B_Tuple      : constant PyObject := To_Tuple (B);
      PyParams     : PyObject;
      PyResult     : PyObject;
      Result       : aliased Interfaces.C.long;
   begin
      Assert (A_Tuple /= Null_Address, Routine_Name & "A_Tuple is null");
      Assert (B_Tuple /= Null_Address, Routine_Name & "B_Tuple is null");

      PyParams :=
        Py_BuildValue (Interfaces.C.To_C ("OO"), A_Tuple, B_Tuple);
      Assert (PyParams /= Null_Address, Routine_Name & "PyParams is null");

      PyResult := Call_Object (PyFunc, PyParams);
      if PyResult = System.Null_Address then
         Put (Routine_Name & "Py error message: ");
         PyErr_Print;
      end if;

      Result := PyInt_AsLong (PyResult);
      Put_Line (Routine_Name & " Result: " & Interfaces.C.long'Image (Result));

      Py_DecRef (PyFunc);
      Py_DecRef (A_Tuple);
      Py_DecRef (B_Tuple);
      Py_DecRef (PyParams);
      Py_DecRef (PyResult);

   end Call;

   --  -------------------------------------------------------------------------

   procedure Call (M : Module; Function_Name : String;
                   A : ML_Arrays_And_Matrices.Integer_Array_List;
                   B : ML_Arrays_And_Matrices.Integer_Array_List;
                   C : ML_Arrays_And_Matrices.Integer_Array_List) is
      use System;
      function Py_BuildValue (Format     : Interfaces.C.char_array;
                              T1, T2, T3 : PyObject)  return PyObject;
      pragma Import (C, Py_BuildValue, "Py_BuildValue");

      Routine_Name : constant String := "Python.Call 3 * Integer_Array_List ";
      PyFunc       : constant PyObject := Get_Symbol (M, Function_Name);
      A_Tuple      : constant PyObject := To_Tuple (A);
      B_Tuple      : constant PyObject := To_Tuple (B);
      C_Tuple      : constant PyObject := To_Tuple (C);
      PyParams     : PyObject;
      PyResult     : PyObject;
      Result       : aliased Interfaces.C.long;
   begin
      Assert (A_Tuple /= Null_Address, Routine_Name & "A_Tuple is null");
      Assert (B_Tuple /= Null_Address, Routine_Name & "B_Tuple is null");
      Assert (C_Tuple /= Null_Address, Routine_Name & "C_Tuple is null");

      PyParams :=
        Py_BuildValue (Interfaces.C.To_C ("OOO"), A_Tuple, B_Tuple, C_Tuple);
      Assert (PyParams /= Null_Address, Routine_Name & "PyParams is null");

      PyResult := Call_Object (PyFunc, PyParams);
      if PyResult = System.Null_Address then
         Put (Routine_Name & "Py error message: ");
         PyErr_Print;
      end if;

      Result := PyInt_AsLong (PyResult);
      Put_Line (Routine_Name & " Result: " & Interfaces.C.long'Image (Result));

      Py_DecRef (PyFunc);
      Py_DecRef (A_Tuple);
      Py_DecRef (B_Tuple);
      Py_DecRef (C_Tuple);
      Py_DecRef (PyParams);
      Py_DecRef (PyResult);

   end Call;

   --  -------------------------------------------------------------------------

   function Call (M : Module; Function_Name : String;
                  A : ML_Arrays_And_Matrices.Integer_Array_List;
                  B : ML_Arrays_And_Matrices.Integer_Array_List;
                  C : ML_Arrays_And_Matrices.Integer_Array_List;
                  D : ML_Arrays_And_Matrices.Integer_Array_List)
                  return float is
      use System;
      function Py_BuildValue (Format         : Interfaces.C.char_array;
                              T1, T2, T3, T4 : PyObject)  return PyObject;
      pragma Import (C, Py_BuildValue, "Py_BuildValue");

      Routine_Name : constant String := "Python.Call 4 * Integer_Array_List ";
      PyFunc       : constant PyObject := Get_Symbol (M, Function_Name);
      A_Tuple      : constant PyObject := To_Tuple (A);
      B_Tuple      : constant PyObject := To_Tuple (B);
      C_Tuple      : constant PyObject := To_Tuple (C);
      D_Tuple      : constant PyObject := To_Tuple (D);
      PyParams     : PyObject;
      PyResult     : PyObject;
      Result       : aliased Interfaces.C.double;
   begin
      Assert (A_Tuple /= Null_Address, Routine_Name & "A_Tuple is null");
      Assert (B_Tuple /= Null_Address, Routine_Name & "B_Tuple is null");
      Assert (C_Tuple /= Null_Address, Routine_Name & "C_Tuple is null");
      Assert (D_Tuple /= Null_Address, Routine_Name & "D_Tuple is null");

      PyParams :=
        Py_BuildValue (Interfaces.C.To_C ("OOOO"),
                       A_Tuple, B_Tuple, C_Tuple, D_Tuple);
      Assert (PyParams /= Null_Address, Routine_Name & "PyParams is null");

      PyResult := Call_Object (PyFunc, PyParams);
      if PyResult = System.Null_Address then
         Put (Routine_Name & "Py error message: ");
         PyErr_Print;
      end if;

      Result := PyFloat_AsDouble (PyResult);
      Put_Line (Routine_Name & " Result: " &
                  Interfaces.C.double'Image (Result));

      Py_DecRef (PyFunc);
      Py_DecRef (A_Tuple);
      Py_DecRef (B_Tuple);
      Py_DecRef (C_Tuple);
      Py_DecRef (D_Tuple);
      Py_DecRef (PyParams);
      Py_DecRef (PyResult);
      
      return Float (Result);

   end Call;

   --  -------------------------------------------------------------------------

   function Run_String (Script : String) return PyObject is
      use System;
      Obj : PyObject;
   begin
      Obj := PyRun_String (Interfaces.C.To_C (Script));
      if Obj = Null_Address then
         Put_Line ("Python.Run_String caused a Python exception!");
      end if;
      
      return Obj;

   end Run_String;

   --  -------------------------------------------------------------------------

end Python;
