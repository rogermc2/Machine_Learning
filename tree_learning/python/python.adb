
--  Based on inspirel_ada-python_demo

with Interfaces.C;

with Ada.Assertions; use Ada.Assertions;
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Unchecked_Conversion;

with API_Binding;
with Python_API; use Python_API;

package body Python is
     
   --     function To_Tuple (Data : ML_Types.Integer_List_2D) return PyObject;
   procedure To_Tuple (Data : NL_Types.Boolean_List_2D; Result: out PyObject);
   procedure To_Tuple (Data  : ML_Types.Bounded_String_List;
                       Result: out PyObject);
   
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
   function Get_Symbol (M : in Module; Function_Name : in String)
                        return PyObject is
      use type System.Address;
      Routine_Name : constant String := "Python.Get_Symbol ";
      PyModule     : constant PyObject := PyObject (M);
      --  PyObject_GetAttrString retrieves the attribute named Function_Name
      --  from the object PyModule. 
      F            : constant PyObject := PyObject_GetAttrString
        (PyModule, Interfaces.C.To_C (Function_Name));
   begin
      Py_DecRef (PyModule);
      if F = System.Null_Address then
         Put_Line (Routine_Name & "Python error message:");
         PyErr_Print;
         raise Interpreter_Error with "Cannot find function " & Function_Name;
      end if;
      
      return F;
      
   end Get_Symbol;
   
   --  -------------------------------------------------------------------------
    
   function Call_Object (F        : in PyObject; Function_Name : in String;
                         PyParams : in PyObject) return PyObject is
      use type System.Address;
      Routine_Name : constant String := "Python.Call_Object ";
      PyResult     : PyObject;
   begin
      Put_Line (Routine_Name & "PyParams size" &
                  Interfaces.C.int'Image (PyObject_Size (PyParams)));  
      PyResult := PyObject_CallObject (F, PyParams);
      if PyResult = System.Null_Address then
         Put_Line (Routine_Name & "Python error message:");
         PyErr_Print;
         raise Interpreter_Error with "Operation " & Function_Name & " did not return expected result";
      end if;
      
      return PyResult;
      
   exception
      when E : others =>
         Put_Line (Routine_Name & "error: " & Exception_Message (E));
         raise Interpreter_Error;
      
   end Call_Object;     
 
   --  -------------------------------------------------------------------------
   --  public operations
   
   procedure Call (M : Module; Function_Name : String) is
      F      : PyObject;
      Result : PyObject;
   begin
      F := Get_Symbol (M, Function_Name);
      Result := PyObject_CallObject (F, System.Null_Address);
      Py_DecRef (F);
      Py_DecRef (Result);
   end Call;

   --  -------------------------------------------------------------------------
    
   function Call (M : Module; Function_Name : String; A : Integer)
                  return Integer is
      
      function Py_BuildValue (Format : Interfaces.C.char_array;
                              A      : Interfaces.C.int) return PyObject;
      pragma Import (C, Py_BuildValue, "Py_BuildValue");

      F        : constant PyObject := Get_Symbol (M, Function_Name);
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
      AB_Pointers : constant API_Pointers := API_Integer_2D (A, B);
      A_Pointers  : constant API_Int_Pointer_Array :=
                      Get_A_Int_Ptrs (AB_Pointers);
      B_Pointers  : constant API_Int_Pointer_Array :=
                      Get_B_Int_Ptrs (AB_Pointers);
      
      function Py_BuildValue (Format : Interfaces.C.char_array;
                              A_Ptrs : API_Int_Pointer_Array;
                              B_Ptrs : API_Int_Pointer_Array) return PyObject;
      pragma Import (C, Py_BuildValue, "Py_BuildValue");

      F        : constant PyObject := Get_Symbol (M, Function_Name);
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
   
   procedure Call (M      : Module; Function_Name : String;
                   Data   : NL_Types.Boolean_List_2D;
                   Labels : ML_Types.Bounded_String_List) is
      use System;
      function Py_BuildValue (Format  : Interfaces.C.char_array;
                              T1, T2  : PyObject)  return PyObject;
      pragma Import (C, Py_BuildValue, "Py_BuildValue");
      
      Routine_Name  : constant String := "Python.Call 2 ";
      F             : constant PyObject := Get_Symbol (M, Function_Name);
      Data_Tuple    : PyObject;
      Labels_Tuple  : PyObject;
      PyParams      : PyObject;
      Result        : PyObject;
   begin
      Put_Line (Routine_Name & "Data size" &
                 Integer'Image (Integer (Data.Length)));
      To_Tuple (Data, Data_Tuple);
      Assert (Data_Tuple /= Null_Address, Routine_Name & "Data_Tuple is null");
      To_Tuple (Labels, Labels_Tuple);
      Assert (Labels_Tuple /= Null_Address, Routine_Name &
                "Labels_Tuple is null");
      Put_Line (Routine_Name & "Data_Tuple size" &
                  Interfaces.C.int'Image (PyTuple_Size (Data_Tuple)));
      Put_Line (Routine_Name & "Labels_Tuple size" &
                  Interfaces.C.int'Image (PyTuple_Size (Labels_Tuple)));   
      
      PyParams :=
        Py_BuildValue (Interfaces.C.To_C ("OO"), Data_Tuple, Labels_Tuple);
      Assert (PyParams /= Null_Address, Routine_Name & "PyParams is null");
      Put_Line (Routine_Name & "PyParams size" &
                  Interfaces.C.int'Image (PyObject_Size (PyParams)));  
                              
      Result := Call_Object (F, Function_Name, PyParams);
      Put_Line (Routine_Name & "PyResult set");
      Put (Routine_Name & "Py error message: ");
      PyErr_Print;
      Py_DecRef (PyParams);
      Py_DecRef (Data_Tuple);
      Py_DecRef (Labels_Tuple);
      Py_DecRef (Result);

   end Call;
   
   --  -------------------------------------------------------------------------
   
   procedure Call (M                        : Module; Function_Name : String;
                   Data                     : NL_Types.Boolean_List_2D;
                   Labels, Words, Pronounce : ML_Types.Bounded_String_List) is
      use Interfaces.C;
      use API_Binding;
      Routine_Name  : constant String := "Python.Call 4 ";
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

      F        : constant PyObject := Get_Symbol (M, Function_Name);
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
   
   --     function Convert_Big_Array (Data : Boolean_Array) return PyObject is
   --        use System;
   --        use Interfaces.C;
   --        Py_List : constant PyObject := PyList_New (Data'Length);
   --        Py_Item : Int;
   --     begin 
   --        if Py_List /= Null_Address then
   --           for index in Data'Range loop
   --              if Data (index) then
   --                 Py_Item := 1;
   --              else
   --                 Py_Item := 0;
   --              end if;
   --              PyList_SetItem (Py_List, int (index), Py_Item);
   --           end loop;
   --        end if;
   --        
   --        return Py_List;
   --        
   --     end Convert_Big_Array;

   --  -------------------------------------------------------------------------
  
--     procedure Pointers_Example is
--  
--        package String_Ptrs is
--          new System.Address_To_Access_Conversions (String);
--        --  Instantiate a package to convert access types to/from addresses.
--        --  This creates a string access type called Object_Pointer.
--  
--        five : aliased String := "5";
--        --  Five is aliased because it will be using access types on it
--  
--        String_Pointer : String_Ptrs.Object_Pointer := five'unchecked_access;
--        --  Unchecked_access needed because five is local to main program.
--        --  If it was global, we could use 'access.
--        --  This is an Ada access all type
--  
--        String_Address : System.Address := Five'Address;
--        --  This is an address in memory, a C pointer
--        --  Addresses can be found with the 'address attribute.
--        --  This is the equivalent of a C pointe
--     begin
--        String_Pointer := String_Ptrs.To_Pointer (String_Address);
--        String_Address := String_Ptrs.To_Address (String_Pointer);
--        --  Convert between Ada and C pointer types.
--  
--     end Pointers_Example;
   
   --  -------------------------------------------------------------------------

   --     function To_Tuple (Data : ML_Types.Integer_List_2D) return PyObject is
   --        use Interfaces.C;
   --        Routine_Name : constant String := "Python.To_Tuple Integer_List_2D ";
   --        Row_Size   : int;
   --        Value      : Integer;
   --        Long_Value : long;
   --        Item       : PyObject;
   --        Result     : constant PyObject := PyTuple_New (int (Data.Length));
   --     begin
   --        for row in Data.First_Index .. Data.Last_Index loop
   --           Row_Size := int (Data (row).Length);
   --           Item := PyTuple_New (Row_Size);
   --           for col in Data (row).First_Index .. Data (row).Last_Index loop
   --              Value := Data (row) (col);
   --              Long_Value := long (Value);
   --              PyTuple_SetItem (Item, int (col), PyLong_FromLong (Long_Value));
   --           end loop;
   --           PyTuple_SetItem (Result, int (row), Item);
   --        end loop;
   --  
   --        return Result;
   --        
   --     exception
   --        when E : others =>
   --           Put_Line (Routine_Name & "error" & Exception_Message (E));
   --           raise Interpreter_Error;
   --           
   --     end To_Tuple;

   --  -------------------------------------------------------------------------

   procedure To_Tuple (Data : NL_Types.Boolean_List_2D;
                      Result : out PyObject) is
      use Interfaces.C;
      Routine_Name : constant String := "Python.To_Tuple Boolean_List_2D ";
      Row_Size     : int;
      Long_Value   : long;
      Item         : PyObject;
   begin
      Result := PyTuple_New (int (Data.Length));
      for row in Data.First_Index .. Data.Last_Index loop
         Row_Size := int (Data (row).Length);
         Item := PyTuple_New (Row_Size);
         for col in Data (row).First_Index .. Data (row).Last_Index loop
            if Data (row) (col) then
               Long_Value := 1;
            else
               Long_Value := 0;
            end if;
            PyTuple_SetItem (Item, int (col), PyLong_FromLong (Long_Value));
         end loop;
         PyTuple_SetItem (Result, int (row), Item);
      end loop;
       
   exception
      when E : others =>
         Put_Line (Routine_Name & "error" &  Exception_Message (E));
         raise Interpreter_Error;
      
   end To_Tuple;

   --  -------------------------------------------------------------------------

   procedure To_Tuple (Data : ML_Types.Bounded_String_List;
                       Result: out PyObject) is
      use Interfaces.C;
      Routine_Name : constant String := "Python.To_Tuple Bounded_String_List ";
   begin
      Result := PyTuple_New (int (Data.Length));
      for row in Data.First_Index .. Data.Last_Index loop
         declare
            Text : constant char_array := To_C (Data (row));
            Item : constant PyObject := PyBytes_FromString (Text);
         begin
            PyTuple_SetItem (Result, int (row), Item);
         end;
      end loop;

   exception
      when E : others =>
         Put_Line (Routine_Name & "error" & Exception_Message (E));
         raise Interpreter_Error;
      
   end To_Tuple;

   --  -------------------------------------------------------------------------

end Python;
