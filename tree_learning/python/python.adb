with Interfaces.C;

with Ada.Text_IO; use Ada.Text_IO;

with API_Binding;

package body Python is

   subtype PyObject is System.Address;

   pragma Warnings (Off, "function ""PyCheck_Tuple"" is not referenced");
   function PyCheck_Tuple (Obj : PyObject) return Interfaces.C.Int;
   pragma Import (C, PyCheck_Tuple, "PyTuple_Check");
   
   procedure Py_DecRef (Obj : PyObject);
   pragma Import (C, Py_DecRef, "Py_DecRef");
          
   pragma Warnings (Off, "procedure ""Py_IncRef"" is not referenced");
   procedure Py_IncRef (Obj : PyObject);
   pragma Import (C, Py_IncRef, "Py_IncRef");

   procedure Py_Initialize;
   pragma Import (C, Py_Initialize, "Py_Initialize");

   function PyInt_AsLong (I : PyObject) return Interfaces.C.long;
   pragma Import (C, PyInt_AsLong, "PyLong_AsLong");
   
   pragma Warnings (Off, "procedure ""PyErr_Print"" is not referenced");
   procedure PyErr_Print;
   pragma Import (C, PyErr_Print, "PyErr_Print");
   
   procedure Py_Finalize;
   pragma Import (C, Py_Finalize, "Py_Finalize");
   
   function PyObject_GetAttrString
     (Obj : PyObject; Name : Interfaces.C.char_array) return PyObject;
   pragma Import (C, PyObject_GetAttrString, "PyObject_GetAttrString");  
   
   function PyImport_Import (Obj : PyObject) return PyObject;
   pragma Import (C, PyImport_Import, "PyImport_Import");
   
   pragma Warnings (Off, "function ""PyList_Check"" is not referenced");
   function PyList_Check (Obj : PyObject) return Interfaces.C.Int;
   pragma Import (C, PyList_Check, "PyList_Check");
   
   function PyObject_CallObject (Obj : PyObject; Args : PyObject)
                                 return PyObject;
   pragma Import (C, PyObject_CallObject, "PyObject_CallObject");
   
   pragma Warnings (Off, "function ""PyParse_Tuple"" is not referenced");
   function PyParse_Tuple (Args : PyObject; Index : Interfaces.C.char_array; Obj : PyObject)
                           return Interfaces.C.int;  --  returns Boolean
   pragma Import (C, PyParse_Tuple, "PyArg_ParseTuple");
     
   function PyRun_SimpleString (Command : Interfaces.C.char_array)
                                return Interfaces.C.int;
   pragma Import (C, PyRun_SimpleString, "PyRun_SimpleString");
   
   procedure Py_SetProgramName (Name : Interfaces.C.char_array);
   pragma Import (C, Py_SetProgramName, "Py_SetProgramName");
    
   function PyString_FromString (Str : Interfaces.C.char_array)
                                 return PyObject;
   pragma Import (C, PyString_FromString, "PyUnicode_FromString");
   
   pragma Warnings (Off, "procedure ""PySys_SetPath"" is not referenced");
   procedure PySys_SetPath (Path : Interfaces.C.char_array);
   pragma Import (C, PySys_SetPath, "PySys_SetPath");
    
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
      
      --  Below: workaround for the following issue:
      --  http://stackoverflow.com/questions/13422206/how-to-load-a-custom-python-module-in-c

      Execute_String ("import sys");
      Execute_String ("import os");
      Execute_String ("sys.path.append('.')");
      Execute_String ("sys.path.append('./src')");
      Execute_String ("sys.path.append('../..')");
      Execute_String ("sys.path.append('../../python')");
      Execute_String ("sys.path.append('../../tree')");
      --        Execute_String
      --          ("sys.path.append('/Applications_Packages/scikit-learn/sklearn/tree')");
      --        PySys_SetPath (To_C ("Applications_Packages/scikit-learn/sklearn/tree"));
      
   end Initialize;
    
   --  -------------------------------------------------------------------------
    
   procedure Finalize is
   begin
      Py_Finalize;
   end Finalize;
    
   --  -------------------------------------------------------------------------
    
   procedure Execute_String (Script : String) is
      Dummy : Interfaces.C.int;
   begin
      Dummy := PyRun_SimpleString (Interfaces.C.To_C (Script));
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
      --        Execute_String ("print ('cwd: ', os.getcwd())");
      --        Execute_String ("print ('cwd files: ', os.listdir(os.getcwd()))");
      --        Execute_String ("print ('Path: ', sys.path)");
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
   
   function Get_Symbol (M : Module; Function_Name : String)
                        return PyObject is
      PyModule : constant PyObject := PyObject (M);
      F        : constant PyObject := PyObject_GetAttrString
        (PyModule, Interfaces.C.To_C (Function_Name));
      use type System.Address;
   begin
      if F = System.Null_Address then
         --  PyErr_Print;
         raise Interpreter_Error with "Cannot find function " & Function_Name;
      end if;
      
      return F;
   end Get_Symbol;
   
   --  -------------------------------------------------------------------------
    
   function Call_Object (F        : PyObject; Function_Name : String;
                         PyParams : PyObject) return PyObject is
      PyResult : PyObject;
      use type System.Address;
   begin
      PyResult := PyObject_CallObject (F, PyParams);
      if PyResult = System.Null_Address then
         Put ("Python.Call_Object ");
         PyErr_Print;
         raise Interpreter_Error with "Python.Call_Object, operation " &
           Function_Name &" did not return expected result";
      end if;
      
      return PyResult;
      
   end Call_Object;     
   
   --  -------------------------------------------------------------------------
   --  public operations
   
   procedure Call (M : Module; Function_Name : String) is
      F      : constant PyObject := Get_Symbol (M, Function_Name);
      Result : PyObject;
   begin
      Result := PyObject_CallObject (F, System.Null_Address);
      Py_DecRef (Result);
   end Call;

   --  -------------------------------------------------------------------------
    
   function Call (M : Module; Function_Name : String; A : Integer)
                  return Integer is
      F : constant PyObject := Get_Symbol (M, Function_Name);
      
      function Py_BuildValue (Format : Interfaces.C.char_array;
                              A      : Interfaces.C.int) return PyObject;
      pragma Import (C, Py_BuildValue, "Py_BuildValue");

      PyParams : PyObject;
      PyResult : PyObject;
      Result   : aliased Interfaces.C.long;
   begin
      PyParams := Py_BuildValue (Interfaces.C.To_C ("(i)"),
                                 Interfaces.C.int (A));
      PyResult := Call_Object (F, Function_Name, PyParams);
      Result := PyInt_AsLong (PyResult);
      Py_DecRef (PyParams);
      Py_DecRef (PyResult);

      return Integer (Result);
      
   end Call;
   
   -- --------------------------------------------------------------------------
 
   function Call (M : Module; Function_Name : String;
                  A : Integer; B : Integer) return Integer is
      F : constant PyObject := Get_Symbol (M, Function_Name);
      
      function Py_BuildValue (Format : Interfaces.C.char_array;
                              A      : Interfaces.C.int;
                              B      : Interfaces.C.int) return PyObject;
      pragma Import (C, Py_BuildValue, "Py_BuildValue");

      PyParams : PyObject;
      PyResult : PyObject;
      Result   : aliased Interfaces.C.long;
   begin
      PyParams := Py_BuildValue (Interfaces.C.To_C ("ii"), Interfaces.C.int (A),
                                 Interfaces.C.int (B));
      PyResult := Call_Object (F, Function_Name, PyParams);
      Result := PyInt_AsLong (PyResult);
      Py_DecRef (PyParams);
      Py_DecRef (PyResult);

      return Integer (Result);
   end Call;
   
   --  -------------------------------------------------------------------------
    
   procedure Call (M    : Module; Function_Name : String;
                   A, B : Integer_Matrix) is
      use API_Binding;
      F           : constant PyObject := Get_Symbol (M, Function_Name);
      AB_Pointers : constant API_Pointers := API_Integer_2D (A, B);
      A_Pointers  : constant API_Int_Pointer_Array :=
                      Get_A_Int_Ptrs (AB_Pointers);
      B_Pointers  : constant API_Int_Pointer_Array :=
                      Get_B_Int_Ptrs (AB_Pointers);
      
      function Py_BuildValue (Format : Interfaces.C.char_array;
                              A_Ptrs : API_Int_Pointer_Array;
                              B_Ptrs : API_Int_Pointer_Array) return PyObject;
      pragma Import (C, Py_BuildValue, "Py_BuildValue");

      PyParams : PyObject;
      PyResult : PyObject;
      Result   : aliased Interfaces.C.long;
   begin
      PyParams :=
        Py_BuildValue (Interfaces.C.To_C ("oo"), A_Pointers, B_Pointers);
                              
      PyResult := Call_Object (F, Function_Name, PyParams);
      Result := PyInt_AsLong (PyResult);
      Py_DecRef (PyParams);
      Py_DecRef (PyResult);

   end Call;
   
   --  -------------------------------------------------------------------------
   
   procedure Call (M                        : Module; Function_Name : String;
                   Data                     : NL_Types.Boolean_List_2D;
                   Labels, Words, Pronounce : ML_Types.Bounded_String_List) is
      use Interfaces.C;
      use API_Binding;
      Routine_Name  : constant String := "Python.Call 4 ";
      F             : constant PyObject := Get_Symbol (M, Function_Name);
      ABCD_Pointers : constant API_4D_Pointers :=
                        API_4D (Data, Labels, Words, Pronounce);
      A_Pointers    : constant API_Boolean_Pointer_Array :=
                        Get_A_Ptrs (ABCD_Pointers);
      B_Pointers    : constant Char_Ptr_Array := Get_B_Ptrs (ABCD_Pointers);
      C_Pointers    : constant Char_Ptr_Array := Get_C_Ptrs (ABCD_Pointers);
      D_Pointers    : constant Char_Ptr_Array := Get_D_Ptrs (ABCD_Pointers);
      
      function Py_BuildValue (Format  : Interfaces.C.char_array;
                              A_Ptrs  : API_Boolean_Pointer_Array;
                              B_Ptrs, C_Ptrs,
                              D_Ptrs  : Char_Ptr_Array)  return PyObject;
      pragma Import (C, Py_BuildValue, "Py_BuildValue");

      PyParams : PyObject;
      PyResult : PyObject;
      Result   : aliased Interfaces.C.long;
   begin
      Put_Line (Routine_Name);
      PyParams :=
        Py_BuildValue (Interfaces.C.To_C ("oooo"), A_Pointers, B_Pointers,
                       C_Pointers, D_Pointers);
      Put_Line (Routine_Name & "PyParams set");
                              
      PyResult := Call_Object (F, Function_Name, PyParams);
      Put_Line (Routine_Name & "PyResult set");
      Result := PyInt_AsLong (PyResult);
      Put_Line (Routine_Name & "Number of correct words:" &
                  long'Image (Result));
      Py_DecRef (PyParams);
      Py_DecRef (PyResult);

   end Call;
   
   --  -------------------------------------------------------------------------
   
end Python;
