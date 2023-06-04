
--  Based on inspirel_ada-python_demo

with Interfaces.C;

with Ada.Assertions; use Ada.Assertions;
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Unchecked_Conversion;

--  with Basic_Printing; use Basic_Printing;
with Parsers;
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
      PyFileName   : constant PyObject_Ptr :=
                       PyString_FromString (Interfaces.C.To_C (File_Name));
   begin
      Execute_String ("cwd = os.getcwd()");
      Execute_String ("os.path.join (cwd, '/src/py_package')");
      Execute_String ("os.chdir(cwd + '/src/py_package')");

      declare
         M  : constant PyObject_Ptr := PyImport_Import (PyFileName); 
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
      Py_DecRef (PyObject_Ptr (M));
   end Close_Module;

   --  -------------------------------------------------------------------------
  
   function Py_String_To_Ada (C_String_Ptr : Python_API.PyObject_Ptr)
                              return String is
      --  Routine_Name    : constant String := "Python.Py_String_To_Ada  ";

      procedure Move_Bytes (dst, src : PyObject_Ptr; Count : Positive);
      pragma Import (C, Move_Bytes, "memcpy");

      function Strlen (C_String_Ptr : PyObject_Ptr) return Natural;
      pragma Import (C, Strlen, "strlen");

      Unicode_Ptr : constant PyObject_Ptr := PyUnicode_AsUTF8 (C_String_Ptr);
      Length      : constant Natural := Strlen (Unicode_Ptr);
   begin
      if Length < 1 then
         return "";
      else
         declare
            Ada_String : String (1..Length);
         begin
            Move_Bytes (Ada_String(1)'address, Unicode_Ptr, Length);
            return Ada_String;
         end;
      end if;

   end Py_String_To_Ada;

   -- --------------------------------------------------------------------------
   --  helpers for use from all overloaded Call subprograms
   function Get_Symbol (M : in Module; Function_Name : in String)
                        return PyObject_Ptr is
      use type System.Address;
      Routine_Name : constant String := "Python.Get_Symbol ";
      PyModule     : constant PyObject_Ptr := PyObject_Ptr (M);
      --  PyObject_GetAttrString retrieves the attribute named Function_Name
      --  from the object PyModule.
      F            : constant PyObject_Ptr :=
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

   function Call_Object (PyFunc : PyObject_Ptr) return PyObject_Ptr is
      use Interfaces.C;
      use type System.Address;
      Routine_Name : constant String := "Support_16A.Call_Object ";
      PyParams     : constant PyObject_Ptr := System.Null_Address;
      PyResult     : PyObject_Ptr;
   begin
      Assert (PyFunc /= System.Null_Address, Routine_Name & "PyFunc is null.");
      Assert (PyCallable_Check (PyFunc) /= 0, Routine_Name &
                "PyCallable_Check is null.");
      PyResult := PyObject_CallObject (PyFunc, PyParams);

      if PyResult = System.Null_Address then
         New_Line;
         Put_Line (Routine_Name & "Python error message:");
         PyErr_Print;
         raise Python.Interpreter_Error with Routine_Name & "failed.";
      end if;

      return PyResult;

   exception
      when E : others =>
         raise Python.Interpreter_Error with Routine_Name & "exception: " &
           Exception_Message (E);

   end Call_Object;

   --  -------------------------------------------------------------------------

   function Call_Object (PyFunc, PyParams : PyObject_Ptr) return PyObject_Ptr is
      use Interfaces.C;
      use type System.Address;
      Routine_Name : constant String := "Python.Call_Object ";
      PyResult     : PyObject_Ptr;
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
      Func     : PyObject_Ptr;
      PyResult : PyObject_Ptr;
   begin
      Func := Get_Symbol (M, Function_Name);
      PyResult := PyObject_CallObject (Func, System.Null_Address);
      
      Py_DecRef (Func);
      Py_DecRef (PyResult);

   end Call;

   --  -------------------------------------------------------------------------

   function Call (M : Module; Function_Name : String)
                  return Python_API.PyObject_Ptr is
      use System;
      Routine_Name : constant String := "Python.Call function only ";
      Func         : PyObject_Ptr;
      PyResult     : PyObject_Ptr;
   begin
      Func := Get_Symbol (M, Function_Name);
      PyResult := PyObject_CallObject (Func, System.Null_Address);
      Assert (PyResult /= Null_Address, Routine_Name & "PyResult is null");
      
      Py_DecRef (Func);
      
      return PyResult;

   end Call;

   --  -------------------------------------------------------------------------

   procedure Call (M : Module; Function_Name : String; A : PyObject_Ptr) is

      function Py_BuildValue (Format : Interfaces.C.char_array;
                              A      : PyObject_Ptr) return PyObject_Ptr;
      pragma Import (C, Py_BuildValue, "Py_BuildValue");

      F        : constant PyObject_Ptr := Get_Symbol (M, Function_Name);
      PyParams : PyObject_Ptr;
      PyResult : PyObject_Ptr;
   begin
      PyParams := Py_BuildValue (Interfaces.C.To_C ("(O)"), A);
      PyResult := Call_Object (F, PyParams);
      
      Py_DecRef (F);
      Py_DecRef (PyParams);
      Py_DecRef (PyResult);

   end Call;

   -- --------------------------------------------------------------------------

   function Call (M : Module; Function_Name : String; A : Python_API.PyObject_Ptr)
                  return Boolean is
      use Interfaces.C;

      function Py_BuildValue (Format : char_array; A : Python_API.PyObject_Ptr)
                              return PyObject_Ptr;
      pragma Import (C, Py_BuildValue, "Py_BuildValue");
      F        : constant PyObject_Ptr := Get_Symbol (M, Function_Name);
      PyParams : PyObject_Ptr;
      PyResult : PyObject_Ptr;
      Result   : Boolean;
   begin
      PyParams := Py_BuildValue (To_C ("(O)"), A);
      PyResult := Call_Object (F, PyParams);
      Result := PyInt_AsLong (PyResult) /= 0;
      
      Py_DecRef (F);
      Py_DecRef (PyParams);
      Py_DecRef (PyResult);

      return Result;

   end Call;

   -- --------------------------------------------------------------------------
 
   function Call (M : Module; Function_Name : String; A : Python_API.PyObject_Ptr)
                  return Integer is
      use Interfaces.C;

      function Py_BuildValue (Format : char_array; A : Python_API.PyObject_Ptr)
                              return PyObject_Ptr;
      pragma Import (C, Py_BuildValue, "Py_BuildValue");
      F        : constant PyObject_Ptr := Get_Symbol (M, Function_Name);
      PyParams : PyObject_Ptr;
      PyResult : PyObject_Ptr;
      Result   : aliased long;
   begin
      PyParams := Py_BuildValue (To_C ("(O)"), A);
      PyResult := Call_Object (F, PyParams);
      Result := PyInt_AsLong (PyResult);
      
      Py_DecRef (F);
      Py_DecRef (PyParams);
      Py_DecRef (PyResult);

      return Integer (Result);

   end Call;

   -- --------------------------------------------------------------------------
 
   procedure Call (M : Module; Function_Name, A : String) is

      function Py_BuildValue (Format : Interfaces.C.char_array;
                              A      : Interfaces.C.char_array) return PyObject_Ptr;
      pragma Import (C, Py_BuildValue, "Py_BuildValue");

      F        : constant PyObject_Ptr := Get_Symbol (M, Function_Name);
      PyParams : PyObject_Ptr;
      PyResult : PyObject_Ptr;
   begin
      PyParams := Py_BuildValue (Interfaces.C.To_C ("(s)"),
                                 Interfaces.C.To_C (A));
      PyResult := Call_Object (F, PyParams);
      
      Py_DecRef (F);
      Py_DecRef (PyParams);
      Py_DecRef (PyResult);

   end Call;

   -- --------------------------------------------------------------------------

   function Call (M : Module; Function_Name, A : String)
                  return Python_API.PyObject_Ptr is

      function Py_BuildValue (Format : Interfaces.C.char_array;
                              A      : Interfaces.C.char_array) return PyObject_Ptr;
      pragma Import (C, Py_BuildValue, "Py_BuildValue");

      F        : constant PyObject_Ptr := Get_Symbol (M, Function_Name);
      PyParams : PyObject_Ptr;
      PyResult : PyObject_Ptr;
   begin
      PyParams := Py_BuildValue (Interfaces.C.To_C ("(s)"),
                                 Interfaces.C.To_C (A));
      PyResult := Call_Object (F, PyParams);
      
      Py_DecRef (F);
      Py_DecRef (PyParams);
      
      return PyResult;

   end Call;

   -- --------------------------------------------------------------------------

   function Call (M : Module; Function_Name : String; A : Float)
                  return Python_API.PyObject_Ptr is
      use Interfaces.C;

      function Py_BuildValue (Format : char_array; A : double) return PyObject_Ptr;
      pragma Import (C, Py_BuildValue, "Py_BuildValue");
      F        : constant PyObject_Ptr := Get_Symbol (M, Function_Name);
      PyParams : PyObject_Ptr;
      PyResult : PyObject_Ptr;
   begin
      PyParams := Py_BuildValue (To_C ("(d)"), double (A));
      PyResult := Call_Object (F, PyParams);
      
      Py_DecRef (F);
      Py_DecRef (PyParams);

      return PyResult;

   end Call;

   -- --------------------------------------------------------------------------

   function Call (M : Module; Function_Name : String; A : Integer)
                  return Integer is
      use Interfaces.C;

      function Py_BuildValue (Format : char_array; A : int) return PyObject_Ptr;
      pragma Import (C, Py_BuildValue, "Py_BuildValue");
      F        : constant PyObject_Ptr := Get_Symbol (M, Function_Name);
      PyParams : PyObject_Ptr;
      PyResult : PyObject_Ptr;
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
                  return Python_API.PyObject_Ptr is
      use Interfaces.C;

      function Py_BuildValue (Format : char_array; A : int) return PyObject_Ptr;
      pragma Import (C, Py_BuildValue, "Py_BuildValue");
      F        : constant PyObject_Ptr := Get_Symbol (M, Function_Name);
      PyParams : PyObject_Ptr;
      PyResult : PyObject_Ptr;
   begin
      PyParams := Py_BuildValue (To_C ("(i)"), int (A));
      PyResult := Call_Object (F, PyParams);
      
      Py_DecRef (F);
      Py_DecRef (PyParams);

      return PyResult;

   end Call;

   -- --------------------------------------------------------------------------

   function Call (M : Module; Function_Name : String; A : Integer;
                  B : ML_Arrays_And_Matrices.Real_Float_Matrix)
                  return Python_API.PyObject_Ptr is
      use Interfaces.C;

      function Py_BuildValue (Format  : char_array; A : int;
                              T1      : PyObject_Ptr) return PyObject_Ptr;
      pragma Import (C, Py_BuildValue, "Py_BuildValue");
      F        : constant PyObject_Ptr := Get_Symbol (M, Function_Name);
      A_Tuple  : constant PyObject_Ptr := To_Tuple (B);
      PyParams : PyObject_Ptr;
      PyResult : PyObject_Ptr;
   begin
      PyParams := Py_BuildValue (To_C ("iO"), int (A), A_Tuple);
      PyResult := Call_Object (F, PyParams);
      
      Py_DecRef (F);
      Py_DecRef (A_Tuple);
      Py_DecRef (PyParams);

      return PyResult;

   end Call;

   -- --------------------------------------------------------------------------

   procedure Call (M : Module; Function_Name : String; A, B : Integer) is

      function Py_BuildValue (Format : Interfaces.C.char_array;
                              A      : Interfaces.C.int;
                              B      : Interfaces.C.int) return PyObject_Ptr;
      pragma Import (C, Py_BuildValue, "Py_BuildValue");

      F        : constant PyObject_Ptr := Get_Symbol (M, Function_Name);
      PyParams : PyObject_Ptr;
      PyResult : PyObject_Ptr;
   begin
      PyParams := Py_BuildValue (Interfaces.C.To_C ("ii"), Interfaces.C.int (A),
                                 Interfaces.C.int (B));
      PyResult := Call_Object (F, PyParams);
      
      Py_DecRef (F);
      Py_DecRef (PyParams);
      Py_DecRef (PyResult);

   end Call;

   --  -------------------------------------------------------------------------

   function Call (M : Module; Function_Name : String; A, B : Integer)
                  return Integer is

      function Py_BuildValue (Format : Interfaces.C.char_array;
                              A      : Interfaces.C.int;
                              B      : Interfaces.C.int) return PyObject_Ptr;
      pragma Import (C, Py_BuildValue, "Py_BuildValue");

      F        : constant PyObject_Ptr := Get_Symbol (M, Function_Name);
      PyParams : PyObject_Ptr;
      PyResult : PyObject_Ptr;
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
                  return Python_API.PyObject_Ptr is

      function Py_BuildValue (Format : Interfaces.C.char_array;
                              A      : Interfaces.C.int;
                              B      : Interfaces.C.double) return PyObject_Ptr;
      pragma Import (C, Py_BuildValue, "Py_BuildValue");

      F        : constant PyObject_Ptr := Get_Symbol (M, Function_Name);
      PyParams : PyObject_Ptr;
      PyResult : PyObject_Ptr;
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
                   A : ML_Arrays_And_Matrices.Integer_Array) is

      function Py_BuildValue (Format  : Interfaces.C.char_array;
                              T1      : PyObject_Ptr) return PyObject_Ptr;
      pragma Import (C, Py_BuildValue, "Py_BuildValue");

      F        : constant PyObject_Ptr := Get_Symbol (M, Function_Name);
      A_Tuple  : constant PyObject_Ptr := To_Tuple (A);
      PyParams : PyObject_Ptr;
      PyResult : PyObject_Ptr;
   begin
      PyParams :=
        Py_BuildValue (Interfaces.C.To_C ("O"), A_Tuple);

      PyResult := Call_Object (F, PyParams);

      Py_DecRef (F);
      Py_DecRef (A_Tuple);
      Py_DecRef (PyParams);
      Py_DecRef (PyResult);

   end Call;

   --  -------------------------------------------------------------------------

   function Call (M : Python.Module; Function_Name : String;
                  A : ML_Arrays_And_Matrices.Integer_Array_List;
                  B : ML_Types.Integer_List) return Python_API.PyObject_Ptr is
      use Interfaces.C;

      function Py_BuildValue (Format : char_array; T1, T2 : PyObject_Ptr)
                              return PyObject_Ptr;
      pragma Import (C, Py_BuildValue, "Py_BuildValue");

      F            : constant PyObject_Ptr := Python.Get_Symbol (M, Function_Name);
      A_Tuple      : constant PyObject_Ptr := To_Tuple (A);
      B_Tuple      : constant PyObject_Ptr := To_Tuple (B);
      PyParams     : PyObject_Ptr;
      PyResult     : PyObject_Ptr;
   begin
      PyParams := Py_BuildValue (To_C ("OO"), A_Tuple, B_Tuple);
      PyResult := Python.Call_Object (F, PyParams);

      Py_DecRef (F);
      Py_DecRef (A_Tuple);
      Py_DecRef (B_Tuple);
      Py_DecRef (PyParams);

      return PyResult;

   end Call;

   -- --------------------------------------------------------------------------

   procedure Call (M    : Module; Function_Name : String;
                   A, B : ML_Types.Integer_List) is
      function Py_BuildValue (Format  : Interfaces.C.char_array;
                              T1, T2  : PyObject_Ptr) return PyObject_Ptr;
      pragma Import (C, Py_BuildValue, "Py_BuildValue");

      F        : constant PyObject_Ptr := Get_Symbol (M, Function_Name);
      A_Tuple  : constant PyObject_Ptr := To_Tuple (A);
      B_Tuple  : constant PyObject_Ptr := To_Tuple (B);
      PyParams : PyObject_Ptr;
      PyResult : PyObject_Ptr;
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

   procedure Call (M : Module; Function_Name : String;
                   A : ML_Types.Integer_List; B : Integer) is
      use Interfaces.C;
      function Py_BuildValue (Format  : Interfaces.C.char_array;
                              T1      : PyObject_Ptr; B : int) return PyObject_Ptr;
      pragma Import (C, Py_BuildValue, "Py_BuildValue");

      F        : constant PyObject_Ptr := Get_Symbol (M, Function_Name);
      A_Tuple  : constant PyObject_Ptr := To_Tuple (A);
      PyParams : PyObject_Ptr;
      PyResult : PyObject_Ptr;
   begin
      PyParams :=
        Py_BuildValue (Interfaces.C.To_C ("Oi"), A_Tuple, int (B));

      PyResult := Call_Object (F, PyParams);

      Py_DecRef (F);
      Py_DecRef (A_Tuple);
      Py_DecRef (PyParams);
      Py_DecRef (PyResult);

   end Call;

   --  -------------------------------------------------------------------------

   function Call (M : Module; Function_Name : String; A : Python_API.PyObject_Ptr)
                  return ML_Arrays_And_Matrices.Integer_Array is
      use ML_Arrays_And_Matrices;
      
      function Py_BuildValue (Format : Interfaces.C.char_array;
                              O1     : PyObject_Ptr) return PyObject_Ptr;
      pragma Import (C, Py_BuildValue, "Py_BuildValue");

      F        : constant PyObject_Ptr := Get_Symbol (M, Function_Name);
      PyParams : PyObject_Ptr;
      PyResult : PyObject_Ptr;
   begin
      PyParams :=
        Py_BuildValue (Interfaces.C.To_C ("(O)"), A);
      
      PyResult := Call_Object (F, PyParams);
      Py_DecRef (F);
      Py_DecRef (PyParams);
      
      declare
         Result : constant Integer_Array := Parsers.Parse_Tuple (PyResult);
      begin
         Py_DecRef (PyResult);
         return Result;
      end;

   end Call;

   --  -------------------------------------------------------------------------

   procedure Call (M : Module; Function_Name : String;
                   A : ML_Arrays_And_Matrices.Integer_Matrix) is

      function Py_BuildValue (Format  : Interfaces.C.char_array;
                              T1      : PyObject_Ptr) return PyObject_Ptr;
      pragma Import (C, Py_BuildValue, "Py_BuildValue");

      F        : constant PyObject_Ptr := Get_Symbol (M, Function_Name);
      A_Tuple  : constant PyObject_Ptr := To_Tuple (A);
      PyParams : PyObject_Ptr;
      PyResult : PyObject_Ptr;
   begin
      PyParams :=
        Py_BuildValue (Interfaces.C.To_C ("O"), A_Tuple);

      PyResult := Call_Object (F, PyParams);

      Py_DecRef (F);
      Py_DecRef (A_Tuple);
      Py_DecRef (PyParams);
      Py_DecRef (PyResult);

   end Call;

   --  -------------------------------------------------------------------------

   procedure Call (M    : Module; Function_Name : String;
                   A, B : ML_Arrays_And_Matrices.Integer_Matrix) is

      function Py_BuildValue (Format  : Interfaces.C.char_array;
                              T1, T2  : PyObject_Ptr) return PyObject_Ptr;
      pragma Import (C, Py_BuildValue, "Py_BuildValue");

      F        : constant PyObject_Ptr := Get_Symbol (M, Function_Name);
      A_Tuple  : constant PyObject_Ptr := To_Tuple (A);
      B_Tuple  : constant PyObject_Ptr := To_Tuple (B);
      PyParams : PyObject_Ptr;
      PyResult : PyObject_Ptr;
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

   procedure Call (M : Module; Function_Name : String;
                   A : ML_Arrays_And_Matrices.Integer_Array;
                   B : ML_Arrays_And_Matrices.Real_Float_List) is
      
      function Py_BuildValue (Format : Interfaces.C.char_array;
                              T1, T2 : PyObject_Ptr) return PyObject_Ptr;
      pragma Import (C, Py_BuildValue, "Py_BuildValue");

      F        : constant PyObject_Ptr := Get_Symbol (M, Function_Name);
      A_Tuple  : constant PyObject_Ptr := To_Tuple (A);
      B_Tuple  : constant PyObject_Ptr := To_Tuple (B);
      PyParams : PyObject_Ptr;
      PyResult : PyObject_Ptr;
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
                   A    : ML_Arrays_And_Matrices.Integer_Array;
                   B, C : ML_Arrays_And_Matrices.Real_Float_List) is
      
      function Py_BuildValue (Format     : Interfaces.C.char_array;
                              T1, T2, T3 : PyObject_Ptr) return PyObject_Ptr;
      pragma Import (C, Py_BuildValue, "Py_BuildValue");

      F        : constant PyObject_Ptr := Get_Symbol (M, Function_Name);
      A_Tuple  : constant PyObject_Ptr := To_Tuple (A);
      B_Tuple  : constant PyObject_Ptr := To_Tuple (B);
      C_Tuple  : constant PyObject_Ptr := To_Tuple (C);
      PyParams : PyObject_Ptr;
      PyResult : PyObject_Ptr;
   begin
      PyParams :=
        Py_BuildValue (Interfaces.C.To_C ("OOO"), A_Tuple, B_Tuple, C_Tuple);

      PyResult := Call_Object (F, PyParams);

      Py_DecRef (F);
      Py_DecRef (A_Tuple);
      Py_DecRef (B_Tuple);
      Py_DecRef (C_Tuple);
      Py_DecRef (PyParams);
      Py_DecRef (PyResult);

   end Call;

   --  -------------------------------------------------------------------------

   procedure Call (M : Module; Function_Name : String;
                   A : ML_Arrays_And_Matrices.Real_Float_List) is
      
      function Py_BuildValue (Format : Interfaces.C.char_array;
                              T1     : PyObject_Ptr) return PyObject_Ptr;
      pragma Import (C, Py_BuildValue, "Py_BuildValue");

      F        : constant PyObject_Ptr := Get_Symbol (M, Function_Name);
      A_Tuple  : constant PyObject_Ptr := To_Tuple (A);
      PyParams : PyObject_Ptr;
      PyResult : PyObject_Ptr;
   begin
      PyParams :=
        Py_BuildValue (Interfaces.C.To_C ("(O)"), A_Tuple);

      PyResult := Call_Object (F, PyParams);

      Py_DecRef (F);
      Py_DecRef (A_Tuple);
      Py_DecRef (PyParams);
      Py_DecRef (PyResult);

   end Call;

   --  -------------------------------------------------------------------------

   procedure Call (M    : Module; Function_Name : String;
                   A    : ML_Types.Integer_List;
                   B, C : ML_Arrays_And_Matrices.Real_Float_List) is
      
      function Py_BuildValue (Format     : Interfaces.C.char_array;
                              T1, T2, T3 : PyObject_Ptr) return PyObject_Ptr;
      pragma Import (C, Py_BuildValue, "Py_BuildValue");

      F        : constant PyObject_Ptr := Get_Symbol (M, Function_Name);
      A_Tuple  : constant PyObject_Ptr := To_Tuple (A);
      B_Tuple  : constant PyObject_Ptr := To_Tuple (B);
      C_Tuple  : constant PyObject_Ptr := To_Tuple (C);
      PyParams : PyObject_Ptr;
      PyResult : PyObject_Ptr;
   begin
      PyParams :=
        Py_BuildValue (Interfaces.C.To_C ("OOO"), A_Tuple, B_Tuple, C_Tuple);

      PyResult := Call_Object (F, PyParams);

      Py_DecRef (F);
      Py_DecRef (A_Tuple);
      Py_DecRef (B_Tuple);
      Py_DecRef (C_Tuple);
      Py_DecRef (PyParams);
      Py_DecRef (PyResult);

   end Call;

   --  -------------------------------------------------------------------------

   function Call (M : Module; Function_Name : String;
                  A : ML_Arrays_And_Matrices.Real_Float_List)
                  return ML_Arrays_And_Matrices.Real_Float_Vector is
      
      function Py_BuildValue (Format : Interfaces.C.char_array;
                              T1     : PyObject_Ptr) return PyObject_Ptr;
      pragma Import (C, Py_BuildValue, "Py_BuildValue");

      F        : constant PyObject_Ptr := Get_Symbol (M, Function_Name);
      A_Tuple  : constant PyObject_Ptr := To_Tuple (A);
      PyParams : PyObject_Ptr;
      PyResult : PyObject_Ptr;
      Result   : ML_Arrays_And_Matrices.Real_Float_Vector
        (1 .. Integer (A.Length));
   begin
      PyParams :=
        Py_BuildValue (Interfaces.C.To_C ("(O)"), A_Tuple);

      PyResult := Call_Object (F, PyParams);
      Result := Parsers.Parse_Tuple (PyResult);

      Py_DecRef (F);
      Py_DecRef (A_Tuple);
      Py_DecRef (PyParams);
      Py_DecRef (PyResult);
      
      return Result;

   end Call;

   --  -------------------------------------------------------------------------

   procedure Call (M    : Module; Function_Name : String;
                   A, B : ML_Arrays_And_Matrices.Real_Float_Matrix) is
      Routine_Name : constant String :=
                       "Python.Parse_Tuple Real_Float_Matrix * 2 ";
      
      function Py_BuildValue (Format  : Interfaces.C.char_array;
                              T1, T2  : PyObject_Ptr) return PyObject_Ptr;
      pragma Import (C, Py_BuildValue, "Py_BuildValue");

      F        : constant PyObject_Ptr := Get_Symbol (M, Function_Name);
      A_Tuple  : constant PyObject_Ptr := To_Tuple (A);
      B_Tuple  : constant PyObject_Ptr := To_Tuple (B);
      PyParams : PyObject_Ptr;
      PyResult : PyObject_Ptr;
   begin
      Assert (A'Length > 0, Routine_Name & "matrix A is empty");
      Assert (B'Length > 0, Routine_Name & "matrix B is empty");
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

   procedure Call (M : Module; Function_Name : String;
                   A : ML_Arrays_And_Matrices.Real_Float_Matrix;
                   B : ML_Arrays_And_Matrices.Boolean_Array) is

      function Py_BuildValue (Format  : Interfaces.C.char_array;
                              T1, T2  : PyObject_Ptr) return PyObject_Ptr;
      pragma Import (C, Py_BuildValue, "Py_BuildValue");

      F        : constant PyObject_Ptr := Get_Symbol (M, Function_Name);
      A_Tuple  : constant PyObject_Ptr := To_Tuple (A);
      B_Tuple  : constant PyObject_Ptr := To_Tuple (B);
      PyParams : PyObject_Ptr;
      PyResult : PyObject_Ptr;
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
                              T1, T2, T3  : PyObject_Ptr) return PyObject_Ptr;
      pragma Import (C, Py_BuildValue, "Py_BuildValue");

      --        Routine_Name : constant String := "Python.Call ABC 2 int + UBL ";
      F        : constant PyObject_Ptr := Get_Symbol (M, Function_Name);
      A_Tuple  : constant PyObject_Ptr := To_Tuple (A);
      B_Tuple  : constant PyObject_Ptr := To_Tuple (B);
      C_Tuple  : constant PyObject_Ptr := To_Tuple (C);
      PyParams : PyObject_Ptr;
      PyResult : PyObject_Ptr;
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
                              A_C, B_C, C_C, D_C : double) return PyObject_Ptr;
      pragma Import (C, Py_BuildValue, "Py_BuildValue");

      F        : constant PyObject_Ptr := Get_Symbol (M, Function_Name);
      A_C      : constant double := double (A);
      B_C      : constant double := double (B);
      C_C      : constant double := double (C);
      D_C      : constant double := double (D);
      PyParams : PyObject_Ptr;
      PyResult : PyObject_Ptr;
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
         A_C, B_C, C_C, D_C : double; E_C : char_array) return PyObject_Ptr;
      pragma Import (C, Py_BuildValue, "Py_BuildValue");

      F        : constant PyObject_Ptr := Get_Symbol (M, Function_Name);
      A_C      : constant double := double (A);
      B_C      : constant double := double (B);
      C_C      : constant double := double (C);
      D_C      : constant double := double (D);
      E_C      : constant char_array := To_C (E);
      PyParams : PyObject_Ptr;
      PyResult : PyObject_Ptr;
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
                              T1     : PyObject_Ptr) return PyObject_Ptr;
      pragma Import (C, Py_BuildValue, "Py_BuildValue");

      F        : constant PyObject_Ptr := Get_Symbol (M, Function_Name);
      A_Tuple  : constant PyObject_Ptr := To_Tuple (A);
      PyParams : constant PyObject_Ptr := 
                   Py_BuildValue (Interfaces.C.To_C ("(O)"), A_Tuple);
      PyResult : PyObject_Ptr;
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
                              T1, T2 : PyObject_Ptr) return PyObject_Ptr;
      pragma Import (C, Py_BuildValue, "Py_BuildValue");

      F        : constant PyObject_Ptr := Get_Symbol (M, Function_Name);
      A_Tuple  : constant PyObject_Ptr := To_Tuple (A);
      B_Tuple  : constant PyObject_Ptr := To_Tuple (B);
      PyParams : constant PyObject_Ptr := 
                   Py_BuildValue (Interfaces.C.To_C ("OO"), A_Tuple, B_Tuple);
      PyResult : PyObject_Ptr;
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
                              T1, T2  : PyObject_Ptr) return PyObject_Ptr;
      pragma Import (C, Py_BuildValue, "Py_BuildValue");

      F        : constant PyObject_Ptr := Get_Symbol (M, Function_Name);
      A_Tuple  : constant PyObject_Ptr := To_Tuple (A);
      B_Tuple  : constant PyObject_Ptr := To_Tuple (B);
      PyParams : PyObject_Ptr;
      PyResult : PyObject_Ptr;
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

   function Call (M    : Module; Function_Name : String;
                  A, B : ML_Arrays_And_Matrices.Real_Float_Matrix;
                  C    : ML_Arrays_And_Matrices.Integer_Array)
                  return ML_Arrays_And_Matrices.Integer_Array is
      use System;
      use Interfaces.C;
      use ML_Arrays_And_Matrices;
      Routine_Name : constant String := "Python.Call RFM2 IA"; 
      
      function Py_BuildValue (Format     : Interfaces.C.char_array;
                              T1, T2, T3 : PyObject_Ptr) return PyObject_Ptr;
      pragma Import (C, Py_BuildValue, "Py_BuildValue");

      F        : constant PyObject_Ptr := Get_Symbol (M, Function_Name);
      A_Tuple  : constant PyObject_Ptr := To_Tuple (A);
      B_Tuple  : constant PyObject_Ptr := To_Tuple (B);
      C_Tuple  : constant PyObject_Ptr := To_Tuple (C);
      PyParams : PyObject_Ptr;
      PyResult : PyObject_Ptr;
      Result   : ML_Arrays_And_Matrices.Integer_Array (B'Range);
   begin
      --        Print_Matrix_Dimensions (Routine_Name & "A", A);
      --        Print_Matrix_Dimensions (Routine_Name & "B", B);
      --        Put_Line (Routine_Name & "C length" & Integer'Image (C'Length));
      PyParams :=
        Py_BuildValue (Interfaces.C.To_C ("OOO"), A_Tuple, B_Tuple, C_Tuple);

      PyResult := Call_Object (F, PyParams);
      --        Assert (PyError_Occurred /= Null_Address, Routine_Name &
      --                  " PyError_Occurred");      
      Assert (PyTuple_Size (PyResult) > 0, Routine_Name &
                " invalid Tuple Size: " & int'Image (PyTuple_Size (PyResult)));
      Result := Parsers.Parse_Tuple (PyResult);

      Py_DecRef (F);
      Py_DecRef (A_Tuple);
      Py_DecRef (B_Tuple);
      Py_DecRef (C_Tuple);
      Py_DecRef (PyParams);
      Py_DecRef (PyResult);
      
      return Result;

   end Call;

   --  -------------------------------------------------------------------------

   procedure Call (M    : Module; Function_Name : String;
                   A    : ML_Arrays_And_Matrices.Real_Float_Vector;
                   B    : ML_Arrays_And_Matrices.Integer_Array) is

      function Py_BuildValue (Format : Interfaces.C.char_array;
                              T1, T2 : PyObject_Ptr) return PyObject_Ptr;
      pragma Import (C, Py_BuildValue, "Py_BuildValue");

      F        : constant PyObject_Ptr := Get_Symbol (M, Function_Name);
      A_Tuple  : constant PyObject_Ptr := To_Tuple (A);
      B_Tuple  : constant PyObject_Ptr := To_Tuple (B);
      PyParams : constant PyObject_Ptr := 
                   Py_BuildValue (Interfaces.C.To_C ("OO"), A_Tuple, B_Tuple);
      PyResult : PyObject_Ptr;
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

   procedure Call (M          : Module; Function_Name : String;
                   A, B, C, D : ML_Arrays_And_Matrices.Real_Float_Vector) is

      function Py_BuildValue (Format          : Interfaces.C.char_array;
                              T1, T2, T3, T4  : PyObject_Ptr) return PyObject_Ptr;
      pragma Import (C, Py_BuildValue, "Py_BuildValue");

      F        : constant PyObject_Ptr := Get_Symbol (M, Function_Name);
      A_Tuple  : constant PyObject_Ptr := To_Tuple (A);
      B_Tuple  : constant PyObject_Ptr := To_Tuple (B);
      C_Tuple  : constant PyObject_Ptr := To_Tuple (C);
      D_Tuple  : constant PyObject_Ptr := To_Tuple (D);
      PyParams : PyObject_Ptr;
      PyResult : PyObject_Ptr;
   begin
      PyParams :=
        Py_BuildValue (Interfaces.C.To_C ("OOOO"),
                       A_Tuple, B_Tuple, C_Tuple, D_Tuple);

      PyResult := Call_Object (F, PyParams);

      Py_DecRef (F);
      Py_DecRef (A_Tuple);
      Py_DecRef (B_Tuple);
      Py_DecRef (C_Tuple);
      Py_DecRef (D_Tuple);
      Py_DecRef (PyParams);
      Py_DecRef (PyResult);

   end Call;

   --  -------------------------------------------------------------------------

   procedure Call (M    : Module; Function_Name : String;
                   A, B : NL_Types.Float_List) is

      function Py_BuildValue (Format  : Interfaces.C.char_array;
                              T1, T2  : PyObject_Ptr) return PyObject_Ptr;
      pragma Import (C, Py_BuildValue, "Py_BuildValue");

      F        : constant PyObject_Ptr := Get_Symbol (M, Function_Name);
      A_Tuple  : constant PyObject_Ptr := To_Tuple (A);
      B_Tuple  : constant PyObject_Ptr := To_Tuple (B);
      PyParams : PyObject_Ptr;
      PyResult : PyObject_Ptr;
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
                              T1, T2, T3, T4  : PyObject_Ptr) return PyObject_Ptr;
      pragma Import (C, Py_BuildValue, "Py_BuildValue");

      Py_Func  : PyObject_Ptr;
      A_Tuple  : constant PyObject_Ptr := To_Tuple (A);
      B_Tuple  : constant PyObject_Ptr := To_Tuple (B);
      C_Tuple  : constant PyObject_Ptr := To_Tuple (C);
      D_Tuple  : constant PyObject_Ptr := To_Tuple (D);
      PyParams : PyObject_Ptr;
      PyResult : PyObject_Ptr;
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
--        Routine_Name : constant String := "Python.Call ABCD out ";
      
      function Py_BuildValue (Format : char_array;
                              T1, T2 : int) return PyObject_Ptr;
      pragma Import (C, Py_BuildValue, "Py_BuildValue");

      Py_Func  : PyObject_Ptr;
      A_Length : constant int := int (A'Length);
      C_Length : constant int := int (C'Length);
      A_Tuple  : PyObject_Ptr;
      B_Tuple  : PyObject_Ptr;
      C_Tuple  : PyObject_Ptr;
      D_Tuple  : PyObject_Ptr;
      PyParams : PyObject_Ptr;
      PyResult : PyObject_Ptr;
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
      
      A := Parsers.Parse_Tuple (A_Tuple);
      B := Parsers.Parse_Tuple (B_Tuple);
      C := Parsers.Parse_Tuple (C_Tuple);
      D := Parsers.Parse_Tuple (D_Tuple);
      
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
                              T1, T2, T3, T4  : PyObject_Ptr) return PyObject_Ptr;
      pragma Import (C, Py_BuildValue, "Py_BuildValue");

      Py_Func  : PyObject_Ptr;
      A_Tuple  : constant PyObject_Ptr := To_Tuple (A);
      B_Tuple  : constant PyObject_Ptr := To_Tuple (B);
      C_Tuple  : constant PyObject_Ptr := To_Tuple (C);
      D_Tuple  : constant PyObject_Ptr := To_Tuple (D);
      PyParams : PyObject_Ptr;
      PyResult : PyObject_Ptr;
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
                              T1, T2  : PyObject_Ptr) return PyObject_Ptr;
      pragma Import (C, Py_BuildValue, "Py_BuildValue");

      F        : constant PyObject_Ptr := Get_Symbol (M, Function_Name);
      A_Tuple  : constant PyObject_Ptr := To_Tuple (A);
      B_Tuple  : constant PyObject_Ptr := To_Tuple (B);
      PyParams : PyObject_Ptr;
      PyResult : PyObject_Ptr;
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
                              T1, T2  : PyObject_Ptr) return PyObject_Ptr;
      pragma Import (C, Py_BuildValue, "Py_BuildValue");

      F        : constant PyObject_Ptr := Get_Symbol (M, Function_Name);
      A_Tuple  : constant PyObject_Ptr := To_Tuple (A);
      B_Tuple  : constant PyObject_Ptr := To_Tuple (B);
      PyParams : PyObject_Ptr;
      PyResult : PyObject_Ptr;
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

   procedure Call (M : Module; Function_Name : String;
                   A : ML_Arrays_And_Matrices.Real_Float_Matrix;
                   B : ML_Arrays_And_Matrices.Unbounded_String_Array) is

      function Py_BuildValue (Format  : Interfaces.C.char_array;
                              T1, T2  : PyObject_Ptr) return PyObject_Ptr;
      pragma Import (C, Py_BuildValue, "Py_BuildValue");

      F        : constant PyObject_Ptr := Get_Symbol (M, Function_Name);
      A_Tuple  : constant PyObject_Ptr := To_Tuple (A);
      B_Tuple  : constant PyObject_Ptr := To_Tuple (B);
      PyParams : PyObject_Ptr;
      PyResult : PyObject_Ptr;
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

   procedure Call (M : Module; Function_Name, Text : String;
                   A : ML_Arrays_And_Matrices.Real_Float_Matrix;
                   B : ML_Arrays_And_Matrices.Unbounded_String_Array) is

      function Py_BuildValue (Format : Interfaces.C.char_array;
                              S1     : Interfaces.C.char_array;
                              T1, T2 : PyObject_Ptr) return PyObject_Ptr;
      pragma Import (C, Py_BuildValue, "Py_BuildValue");

      Text_C   : constant Interfaces.C.char_array := Interfaces.C.To_C (Text);
      F        : constant PyObject_Ptr := Get_Symbol (M, Function_Name);
      A_Tuple  : constant PyObject_Ptr := To_Tuple (A);
      B_Tuple  : constant PyObject_Ptr := To_Tuple (B);
      PyParams : PyObject_Ptr;
      PyResult : PyObject_Ptr;
   begin
      PyParams :=
        Py_BuildValue (Interfaces.C.To_C ("sOO"), Text_C, A_Tuple, B_Tuple);
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
                   B    : ML_Arrays_And_Matrices.Unbounded_String_Matrix) is

      function Py_BuildValue (Format  : Interfaces.C.char_array;
                              T1, T2  : PyObject_Ptr) return PyObject_Ptr;
      pragma Import (C, Py_BuildValue, "Py_BuildValue");

      F        : constant PyObject_Ptr := Get_Symbol (M, Function_Name);
      A_Tuple  : constant PyObject_Ptr := To_Tuple (A);
      B_Tuple  : constant PyObject_Ptr := To_Tuple (B);
      PyParams : PyObject_Ptr;
      PyResult : PyObject_Ptr;
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

   procedure Call (M : Module; Function_Name : String;
                   A : ML_Arrays_And_Matrices.Real_Float_Vector) is

      function Py_BuildValue (Format : Interfaces.C.char_array;
                              T1     : PyObject_Ptr) return PyObject_Ptr;
      pragma Import (C, Py_BuildValue, "Py_BuildValue");

      F        : constant PyObject_Ptr := Get_Symbol (M, Function_Name);
      A_Tuple  : constant PyObject_Ptr := To_Tuple (A);
      PyParams : PyObject_Ptr;
      PyResult : PyObject_Ptr;
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
                              T1, T2, T3 : PyObject_Ptr) return PyObject_Ptr;
      pragma Import (C, Py_BuildValue, "Py_BuildValue");

      F        : constant PyObject_Ptr := Get_Symbol (M, Function_Name);
      A_Tuple  : constant PyObject_Ptr := To_Tuple (A);
      B_Tuple  : constant PyObject_Ptr := To_Tuple (B);
      C_Tuple  : constant PyObject_Ptr := To_Tuple (C);
      PyParams : PyObject_Ptr;
      PyResult : PyObject_Ptr;
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
                   A : ML_Arrays_And_Matrices.Real_Float_Vector;
                   B : ML_Arrays_And_Matrices.Real_Float_List) is

      function Py_BuildValue (Format : Interfaces.C.char_array;
                              T1, T2 : PyObject_Ptr) return PyObject_Ptr;
      pragma Import (C, Py_BuildValue, "Py_BuildValue");

      F        : constant PyObject_Ptr := Get_Symbol (M, Function_Name);
      A_Tuple  : constant PyObject_Ptr := To_Tuple (A);
      B_Tuple  : constant PyObject_Ptr := To_Tuple (B);
      PyParams : PyObject_Ptr;
      PyResult : PyObject_Ptr;
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
                   A, B : ML_Arrays_And_Matrices.Real_Float_Vector;
                   C    : ML_Arrays_And_Matrices.Real_Float_Matrix) is

      function Py_BuildValue (Format     : Interfaces.C.char_array;
                              T1, T2, T3 : PyObject_Ptr) return PyObject_Ptr;
      pragma Import (C, Py_BuildValue, "Py_BuildValue");

      F        : constant PyObject_Ptr := Get_Symbol (M, Function_Name);
      A_Tuple  : constant PyObject_Ptr := To_Tuple (A);
      B_Tuple  : constant PyObject_Ptr := To_Tuple (B);
      C_Tuple  : constant PyObject_Ptr := To_Tuple (C);
      PyParams : PyObject_Ptr;
      PyResult : PyObject_Ptr;
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
                   A : NL_Types.Boolean_List_2D;
                   B : ML_Types.Bounded_String_List) is
      use System;
      function Py_BuildValue (Format : Interfaces.C.char_array;
                              T1, T2 : PyObject_Ptr)  return PyObject_Ptr;
      pragma Import (C, Py_BuildValue, "Py_BuildValue");

      Routine_Name : constant String := "Python.Call 2 ";
      PyFunc       : constant PyObject_Ptr := Get_Symbol (M, Function_Name);
      A_Tuple      : constant PyObject_Ptr := To_Tuple (A);
      B_Tuple      : constant PyObject_Ptr := To_Tuple (B);
      PyParams     : PyObject_Ptr;
      PyResult     : PyObject_Ptr;
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
                   A : ML_Arrays_And_Matrices.Real_Float_List_2D) is
      use System;
      function Py_BuildValue (Format : Interfaces.C.char_array;
                              T1     : PyObject_Ptr)  return PyObject_Ptr;
      pragma Import (C, Py_BuildValue, "Py_BuildValue");

      Routine_Name : constant String := "Python.Call RF2D ";
      PyFunc       : constant PyObject_Ptr := Get_Symbol (M, Function_Name);
      A_Tuple      : constant PyObject_Ptr := To_Tuple (A);
      PyParams     : PyObject_Ptr;
      PyResult     : PyObject_Ptr;
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

      Py_DecRef (PyFunc);
      Py_DecRef (A_Tuple);
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
                              T1, T2, T3  : PyObject_Ptr)  return PyObject_Ptr;
      pragma Import (C, Py_BuildValue, "Py_BuildValue");

      Routine_Name : constant String := "Python.Call 2 ";
      PyFunc       : constant PyObject_Ptr := Get_Symbol (M, Function_Name);
      A_Tuple      : constant PyObject_Ptr := To_Tuple (A);
      B_Tuple      : constant PyObject_Ptr := To_Tuple (B);
      C_Tuple      : constant PyObject_Ptr := To_Tuple (C);
      PyParams     : PyObject_Ptr;
      PyResult     : PyObject_Ptr;
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
                              T1, T2, T3, T4 : PyObject_Ptr)  return PyObject_Ptr;
      pragma Import (C, Py_BuildValue, "Py_BuildValue");

      Routine_Name : constant String := "Python.Call 4 ";
      PyFunc       : constant PyObject_Ptr := Get_Symbol (M, Function_Name);
      A_Tuple      : constant PyObject_Ptr := To_Tuple (A);
      B_Tuple      : constant PyObject_Ptr := To_Tuple (B);
      C_Tuple      : constant PyObject_Ptr := To_Tuple (C);
      D_Tuple      : constant PyObject_Ptr := To_Tuple (D);
      PyParams     : PyObject_Ptr;
      PyResult     : PyObject_Ptr;
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
                              T1     : PyObject_Ptr)  return PyObject_Ptr;
      pragma Import (C, Py_BuildValue, "Py_BuildValue");

      Routine_Name : constant String := "Python.Call Integer_Array_List ";
      PyFunc       : constant PyObject_Ptr := Get_Symbol (M, Function_Name);
      A_Tuple      : constant PyObject_Ptr := To_Tuple (A);
      PyParams     : PyObject_Ptr;
      PyResult     : PyObject_Ptr;
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
                              T1, T2  : PyObject_Ptr)  return PyObject_Ptr;
      pragma Import (C, Py_BuildValue, "Py_BuildValue");

      Routine_Name : constant String := "Python.Call 2 * Integer_Array_List ";
      PyFunc       : constant PyObject_Ptr := Get_Symbol (M, Function_Name);
      A_Tuple      : constant PyObject_Ptr := To_Tuple (A);
      B_Tuple      : constant PyObject_Ptr := To_Tuple (B);
      PyParams     : PyObject_Ptr;
      PyResult     : PyObject_Ptr;
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
                              T1, T2, T3 : PyObject_Ptr)  return PyObject_Ptr;
      pragma Import (C, Py_BuildValue, "Py_BuildValue");

      Routine_Name : constant String := "Python.Call 3 * Integer_Array_List ";
      PyFunc       : constant PyObject_Ptr := Get_Symbol (M, Function_Name);
      A_Tuple      : constant PyObject_Ptr := To_Tuple (A);
      B_Tuple      : constant PyObject_Ptr := To_Tuple (B);
      C_Tuple      : constant PyObject_Ptr := To_Tuple (C);
      PyParams     : PyObject_Ptr;
      PyResult     : PyObject_Ptr;
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
                              T1, T2, T3, T4 : PyObject_Ptr)  return PyObject_Ptr;
      pragma Import (C, Py_BuildValue, "Py_BuildValue");

      Routine_Name : constant String := "Python.Call 4 * Integer_Array_List ";
      PyFunc       : constant PyObject_Ptr := Get_Symbol (M, Function_Name);
      A_Tuple      : constant PyObject_Ptr := To_Tuple (A);
      B_Tuple      : constant PyObject_Ptr := To_Tuple (B);
      C_Tuple      : constant PyObject_Ptr := To_Tuple (C);
      D_Tuple      : constant PyObject_Ptr := To_Tuple (D);
      PyParams     : PyObject_Ptr;
      PyResult     : PyObject_Ptr;
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

   procedure Call (M : Module; Function_Name : String;
                   A : ML_Arrays_And_Matrices.Real_Vector_List) is
      use System;
      function Py_BuildValue (Format : Interfaces.C.char_array;
                              T1     : PyObject_Ptr)  return PyObject_Ptr;
      pragma Import (C, Py_BuildValue, "Py_BuildValue");

      Routine_Name : constant String := "Python.Call Real_Vector_List ";
      PyFunc       : constant PyObject_Ptr := Get_Symbol (M, Function_Name);
      A_Tuple      : constant PyObject_Ptr := To_Tuple (A);
      PyParams     : PyObject_Ptr;
      PyResult     : PyObject_Ptr;
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

      Py_DecRef (PyFunc);
      Py_DecRef (A_Tuple);
      Py_DecRef (PyParams);
      Py_DecRef (PyResult);

   end Call;

   --  -------------------------------------------------------------------------

   procedure Call (M    : Module; Function_Name : String;
                   A, B : ML_Arrays_And_Matrices.Real_Vector_List) is
      use System;
      function Py_BuildValue (Format     : Interfaces.C.char_array;
                              T1, T2     : PyObject_Ptr)  return PyObject_Ptr;
      pragma Import (C, Py_BuildValue, "Py_BuildValue");

      Routine_Name : constant String := "Python.Call 2 * Real_Vector_List ";
      PyFunc       : constant PyObject_Ptr := Get_Symbol (M, Function_Name);
      A_Tuple      : constant PyObject_Ptr := To_Tuple (A);
      B_Tuple      : constant PyObject_Ptr := To_Tuple (B);
      PyParams     : PyObject_Ptr;
      PyResult     : PyObject_Ptr;
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

      Py_DecRef (PyFunc);
      Py_DecRef (A_Tuple);
      Py_DecRef (B_Tuple);
      Py_DecRef (PyParams);
      Py_DecRef (PyResult);

   end Call;

   --  -------------------------------------------------------------------------

   procedure Call (M    : Module; Function_Name : String;
                   A, B : ML_Arrays_And_Matrices.Real_Float_List) is
      use System;
      function Py_BuildValue (Format     : Interfaces.C.char_array;
                              T1, T2     : PyObject_Ptr)  return PyObject_Ptr;
      pragma Import (C, Py_BuildValue, "Py_BuildValue");

      Routine_Name : constant String := "Python.Call 2 * Real_Vector_List ";
      PyFunc       : constant PyObject_Ptr := Get_Symbol (M, Function_Name);
      A_Tuple      : constant PyObject_Ptr := To_Tuple (A);
      B_Tuple      : constant PyObject_Ptr := To_Tuple (B);
      PyParams     : PyObject_Ptr;
      PyResult     : PyObject_Ptr;
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

      Py_DecRef (PyFunc);
      Py_DecRef (A_Tuple);
      Py_DecRef (B_Tuple);
      Py_DecRef (PyParams);
      Py_DecRef (PyResult);

   end Call;

   --  -------------------------------------------------------------------------

   function Call (M : Module; Function_Name : String; A : Unbounded_String)
                  return Python_API.PyObject_Ptr is
      use System;
      function Py_BuildValue (Format, S1 : Interfaces.C.char_array)
                              return PyObject_Ptr;
      pragma Import (C, Py_BuildValue, "Py_BuildValue");

      Routine_Name : constant String := "Python.Call Unbounded_String_Array ";
      PyFunc       : constant PyObject_Ptr := Get_Symbol (M, Function_Name);
      A_String     : constant Interfaces.C.char_array :=
                       Interfaces.C.To_C (To_String (A));
      PyParams     : PyObject_Ptr;
      PyResult     : PyObject_Ptr;
   begin
      PyParams :=
        Py_BuildValue (Interfaces.C.To_C ("(s)"), A_String);
      Assert (PyParams /= Null_Address, Routine_Name & "PyParams is null");

      PyResult := Call_Object (PyFunc, PyParams);
      if PyResult = System.Null_Address then
         Put (Routine_Name & "Py error message: ");
         PyErr_Print;
      end if;

      Py_DecRef (PyFunc);
      Py_DecRef (PyParams);
      
      return (PyResult);

   end Call;

   --  -------------------------------------------------------------------------

   procedure Call (M : Module; Function_Name : String;
                   A : ML_Arrays_And_Matrices.Unbounded_String_Array) is
      use System;
      function Py_BuildValue (Format : Interfaces.C.char_array;
                              T1     : PyObject_Ptr)  return PyObject_Ptr;
      pragma Import (C, Py_BuildValue, "Py_BuildValue");

      Routine_Name : constant String := "Python.Call Unbounded_String_Array ";
      PyFunc       : constant PyObject_Ptr := Get_Symbol (M, Function_Name);
      A_Tuple      : constant PyObject_Ptr := To_Tuple (A);
      PyParams     : PyObject_Ptr;
      PyResult     : PyObject_Ptr;
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

      Py_DecRef (PyFunc);
      Py_DecRef (A_Tuple);
      Py_DecRef (PyParams);
      Py_DecRef (PyResult);

   end Call;

   --  -------------------------------------------------------------------------

   procedure Call (M : Module; Function_Name : String;
                   A : ML_Arrays_And_Matrices.Unbounded_String_Matrix) is
      use System;
      function Py_BuildValue (Format : Interfaces.C.char_array;
                              T1     : PyObject_Ptr)  return PyObject_Ptr;
      pragma Import (C, Py_BuildValue, "Py_BuildValue");

      Routine_Name : constant String := "Python.Call Unbounded_String_Array ";
      PyFunc       : constant PyObject_Ptr := Get_Symbol (M, Function_Name);
      A_Tuple      : constant PyObject_Ptr := To_Tuple (A);
      PyParams     : PyObject_Ptr;
      PyResult     : PyObject_Ptr;
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

      Py_DecRef (PyFunc);
      Py_DecRef (A_Tuple);
      Py_DecRef (PyParams);
      Py_DecRef (PyResult);

   end Call;

   --  -------------------------------------------------------------------------

   function Run_String (Script : String) return PyObject_Ptr is
      use System;
      Obj : PyObject_Ptr;
   begin
      Obj := PyRun_String (Interfaces.C.To_C (Script));
      if Obj = Null_Address then
         Put_Line ("Python.Run_String caused a Python exception!");
      end if;
      
      return Obj;

   end Run_String;

   --  -------------------------------------------------------------------------

end Python;
