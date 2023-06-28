
with System;

with Interfaces.C;

with Ada.Assertions; use Ada.Assertions;
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;

--  with Basic_Printing; use Basic_Printing;
with Python_API; use Python_API;

package body Python_21A is

   function Parse (Tuple : Python_API.PyObject_Ptr) return Float;
   function To_Tuple (Data : Support_21A.Float_Tensor)  return PyObject_Ptr;

   --  -------------------------------------------------------------------------

   function Call (M : Python.Module; Function_Name : String;
                  Q : Support_21A.Float_Tensor) return Float is
      use Interfaces.C;
      Routine_Name    : constant String := "Python_21A.Call  ";

      function Py_BuildValue (Format : char_array; O1 : PyObject_Ptr)
                              return PyObject_Ptr;
      pragma Import (C, Py_BuildValue, "Py_BuildValue");

      F        : constant PyObject_Ptr := Python.Get_Symbol (M, Function_Name);
      Q_Tuple  : constant PyObject_Ptr := To_Tuple (Q);
      PyParams : PyObject_Ptr;
      PyResult : PyObject_Ptr;
      Result   : Float;
   begin
      Put_Line (Routine_Name);
      PyParams := Py_BuildValue (To_C ("(O)"), Q_Tuple);
      PyResult := Python.Call_Object (F, PyParams);
      Py_DecRef (F);
      Py_DecRef (PyParams);

      Put_Line (Routine_Name & "Parse_Occurrences_Dictionary");
      Result := Parse (PyResult);
      Put_Line (Routine_Name & "Occurrences_Dictionary parsed");
      Py_DecRef (PyResult);

      return Result;

   end Call;

   -- --------------------------------------------------------------------------

   function Parse (Tuple : Python_API.PyObject_Ptr) return Float is
      use System;
      use Interfaces.C;
      Routine_Name    : constant String := "Python_21A.Parse_Tuple ";
      Tuple_Size      : constant int := PyTuple_Size (Tuple);
      Tuple_Item      : PyObject_Ptr;
      Tuple_Item_Size : Integer;
      Py_Str_Ptr      : PyObject_Ptr;
      Result          : Float;
   begin
      Assert (Tuple /= System.Null_Address, Routine_Name & "Tuple is null.");

      Py_DecRef (Tuple_Item);
      Py_DecRef (Py_Str_Ptr);

      return Result;

   end Parse;

   -- --------------------------------------------------------------------------

   function To_Tuple (Data : Support_21A.Float_Tensor)  return PyObject_Ptr is
      use Interfaces.C;
      Routine_Name  : constant String := "Python_21a.To_Tuple ";
      Result        : constant PyObject_Ptr := PyTuple_New (int (Data'Length));
      Py_Tensor     : PyObject_Ptr;
      Py_Index      : int := -1;
   begin
      for mat in Data'Range loop
         Py_Index := Py_Index + 1;
         Py_Tensor := To_Tuple (Data);
         PyTuple_SetItem (Result, Py_Index, Py_Matrix);
      end loop;

      return Result;

   exception
      when E : others =>
         Put_Line (Routine_Name & "error" & Exception_Message (E));
         raise;

   end To_Tuple;

   --  -------------------------------------------------------------------------

end Python_21A;
