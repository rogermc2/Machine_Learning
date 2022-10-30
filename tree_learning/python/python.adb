with Interfaces.C;

package body Python is

   subtype PyObject is System.Address;

   procedure Py_SetProgramName (Name : in Interfaces.C.char_array);
   pragma Import (C, Py_SetProgramName, "Py_SetProgramName");

   procedure Py_Initialize;
   pragma Import (C, Py_Initialize, "Py_Initialize");

   procedure Py_Finalize;
   pragma Import (C, Py_Finalize, "Py_Finalize");
    
   function PyRun_SimpleString (Command : in Interfaces.C.char_array)
                                return Interfaces.C.int;
   pragma Import (C, PyRun_SimpleString, "PyRun_SimpleString");
    
   pragma Warnings (Off, "procedure ""Py_IncRef"" is not referenced");
   procedure Py_IncRef (Obj : in PyObject);
   pragma Import (C, Py_IncRef, "Py_IncRef");
    
   procedure Py_DecRef (Obj : in PyObject);
   pragma Import (C, Py_DecRef, "Py_DecRef");
    
   function PyInt_AsLong (I : in PyObject) return Interfaces.C.long;
   pragma Import (C, PyInt_AsLong, "PyLong_AsLong");
      
   function PyString_FromString (Str : in Interfaces.C.char_array)
                                 return PyObject;
   pragma Import (C, PyString_FromString, "PyUnicode_FromString");
    
   function PyImport_Import (Obj : in PyObject) return PyObject;
   pragma Import (C, PyImport_Import, "PyImport_Import");
   
   function PyObject_GetAttrString
     (Obj : in PyObject; Name : in Interfaces.C.char_array) return PyObject;
   pragma Import (C, PyObject_GetAttrString, "PyObject_GetAttrString");
   
   function PyObject_CallObject (Obj : in PyObject; Args : in PyObject)
                                 return PyObject;
   pragma Import (C, PyObject_CallObject, "PyObject_CallObject");
   
   pragma Warnings (Off, "function ""PyList_Check"" is not referenced");
   function PyList_Check (Obj : in PyObject) return Interfaces.C.Int;
   pragma Import (C, PyList_Check, "PyList_Check");
   
   pragma Warnings (Off, "function ""PyCheck_Tuple"" is not referenced");
   function PyCheck_Tuple (Obj : in PyObject) return Interfaces.C.Int;
   pragma Import (C, PyCheck_Tuple, "PyTuple_Check");
   
   pragma Warnings (Off, "procedure ""PyErr_Print"" is not referenced");
   procedure PyErr_Print;
   pragma Import (C, PyErr_Print, "PyErr_Print");
   
   pragma Warnings (Off, "procedure ""PySys_SetPath"" is not referenced");
   procedure PySys_SetPath (Path : Interfaces.C.char_array);
   pragma Import (C, PySys_SetPath, "PySys_SetPath");
    
   --  -------------------------------------------------------------------------
    
   procedure Initialize (Program_Name : in String := "") is
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
      Execute_String ("sys.path.append('.')");
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
    
   procedure Execute_String (Script : in String) is
      Dummy : Interfaces.C.int;
   begin
      Dummy := PyRun_SimpleString (Interfaces.C.To_C (Script));
   end Execute_String;
    
   --  -------------------------------------------------------------------------
    
   function Import_File (File_Name : in String) return Module is
      use type System.Address;
      PyFileName : constant PyObject :=
                     PyString_FromString (Interfaces.C.To_C (File_Name));
      M          : constant PyObject := PyImport_Import (PyFileName);  
   begin
      Py_DecRef (PyFileName);
      if M = System.Null_Address then
         --  PyErr_Print;
         raise Interpreter_Error with "Cannot load module from file " &
           File_Name;
      end if;
       
      return Module (M);
   end Import_File;
   
   --  -------------------------------------------------------------------------
    
   procedure Close_Module (M : in Module) is
   begin
      Py_DecRef (PyObject (M));
   end Close_Module;

   --  -------------------------------------------------------------------------   
   --  helpers for use from all overloaded Call subprograms
   
   function Get_Symbol (M : in Module; Function_Name : in String)
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
    
   function Call_Object (F        : in PyObject; Function_Name : in String;
                         PyParams : in PyObject) return PyObject is
      PyResult : PyObject;
      use type System.Address;
   begin
      PyResult := PyObject_CallObject (F, PyParams);
      if PyResult = System.Null_Address then
         raise Interpreter_Error with "Operation " & Function_Name &
           " did not return expected result";
      end if;
      
      return PyResult;
      
   end Call_Object;     
   
   --  -------------------------------------------------------------------------
     --  public operations
   
   procedure Call (M : in Module; Function_Name : in String) is
      F      : constant PyObject := Get_Symbol (M, Function_Name);
      Result : PyObject;
   begin
      Result := PyObject_CallObject (F, System.Null_Address);
      Py_DecRef (Result);
   end Call;

   --  -------------------------------------------------------------------------
    
   function Call (M : in Module; Function_Name : in String; A : in Integer)
                  return Integer is
      F : constant PyObject := Get_Symbol (M, Function_Name);
      
      function Py_BuildValue (Format : in Interfaces.C.char_array;
                              A      : in Interfaces.C.int) return PyObject;
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
 
   function Call (M : in Module; Function_Name : in String;
                  A : in Integer; B : Integer) return Integer is
      F : constant PyObject := Get_Symbol (M, Function_Name);
      
      function Py_BuildValue (Format : in Interfaces.C.char_array;
                              A      : in Interfaces.C.int;
                              B      : in Interfaces.C.int) return PyObject;
      pragma Import (C, Py_BuildValue, "Py_BuildValue");

      PyParams : PyObject;
      PyResult : PyObject;
      Result : aliased Interfaces.C.long;
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
    
end Python;
