with System;

with Interfaces.C;

package Python_API is
   
   subtype PyObject is System.Address;
   
   function PyArray_SimpleNewFromData (Obj : PyObject) return Interfaces.C.Int;
   pragma Import (C, PyArray_SimpleNewFromData, "PyArray_SimpleNewFromData");

   function PyBytes_AsString (Text : Interfaces.C.char_array) return PyObject;
   pragma Import (C, PyBytes_AsString, "PyBytes_AsString");
   
   function PyBytes_FromString (Text : Interfaces.C.char_array) return PyObject;
   pragma Import (C, PyBytes_FromString, "PyBytes_FromString");

   function PyCheck_Tuple (Obj : PyObject) return Interfaces.C.Int;
   pragma Import (C, PyCheck_Tuple, "PyTuple_Check");
   
   procedure Py_DecRef (Obj : PyObject);
   pragma Import (C, Py_DecRef, "Py_DecRef");
          
   procedure Py_IncRef (Obj : PyObject);
   pragma Import (C, Py_IncRef, "Py_IncRef");

   procedure Py_Initialize;
   pragma Import (C, Py_Initialize, "Py_Initialize");

   function PyInt_AsLong (I : PyObject) return Interfaces.C.long;
   pragma Import (C, PyInt_AsLong, "PyLong_AsLong");
   
   procedure PyErr_Print;
   pragma Import (C, PyErr_Print, "PyErr_Print");
   
   procedure Py_Finalize;
   pragma Import (C, Py_Finalize, "Py_Finalize");
      
   function PyImport_Import (Obj : PyObject) return PyObject;
   pragma Import (C, PyImport_Import, "PyImport_Import");
   
   function PyList_Check (Obj : PyObject) return Interfaces.C.Int;
   pragma Import (C, PyList_Check, "PyList_Check");
   
   function PyList_New (Length : Interfaces.C.int) return PyObject;
   pragma Import (C, PyList_New, "PyList_New");
   
   procedure PyList_SetItem (List : PyObject; Position : Interfaces.C.int;
                             Item : Interfaces.C.Int);
   pragma Import (C, PyList_SetItem, "PyList_SetItem");  
   
   function PyLong_FromLong (Val : Interfaces.C.long) return PyObject;
   pragma Import (C, PyLong_FromLong, "PyLong_FromLong");
   
   function PyObject_GetAttrString
     (Obj : PyObject; Name : Interfaces.C.char_array) return PyObject;
   pragma Import (C, PyObject_GetAttrString, "PyObject_GetAttrString");  
   
   function PyObject_CallObject (Obj : PyObject; Args : PyObject)
                                 return PyObject;
   pragma Import (C, PyObject_CallObject, "PyObject_CallObject");
   
   function PyObject_String (Obj : PyObject) return PyObject;
   pragma Import (C, PyObject_String, "PyObject_Str");
   
   --  args is a C array consisting of the positional arguments followed by the
   --  values of the keyword arguments which can be NULL if there are no arguments
   --     function PyObject_VectorCall (Obj       : PyObject; Args : PyObject;
   --                                   Key_Words : PyObject := System.Null_Address)
   --                                   return PyObject;
   --     pragma Import (C, PyObject_VectorCall, "PyObject_Vectorcall");
   
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
   
   procedure PySys_SetPath (Path : Interfaces.C.char_array);
   pragma Import (C, PySys_SetPath, "PySys_SetPath");
   
   function PyTuple_New (Length : Interfaces.C.int) return PyObject;
   pragma Import (C, PyTuple_New, "PyTuple_New");
   
   procedure PyTuple_SetItem (Tuple : PyObject; Pos : Interfaces.C.int;
                              Item  : PyObject);
   pragma Import (C, PyTuple_SetItem, "PyTuple_SetItem");

end Python_API;
