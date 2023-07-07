
with Interfaces.C; use Interfaces.C;

with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;

--  with Basic_Printing; use Basic_Printing;
with Parsers;
with Python_API; use Python_API;
with Tuple_Builder;

package body Python_21A is

   function To_Tuple (Data : Support_21A.Float_Tensor)  return PyObject_Ptr;

   --  -------------------------------------------------------------------------

   function Call (M : Python.Module; Function_Name : String;
                  Q : Support_21A.Float_Tensor) return Float is
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
      Py_DecRef (Q_Tuple);
      Py_DecRef (PyParams);

      Result := Float (Python_API.PyFloat_AsDouble (PyResult));
      Py_DecRef (PyResult);

      return Result;

   end Call;

   -- --------------------------------------------------------------------------

   procedure Parse_Tuple (Tuple : PyObject_Ptr;
                          Pi, Q : out Real_Float_Matrix) is
      --        Routine_Name : constant String := "Parsers.Parse_Tuple ";
   begin
      Pi := Parsers.Parse_Tuple (PyTuple_GetItem (Tuple, 0));
      Q := Parsers.Parse_Tuple (PyTuple_GetItem (Tuple, 1));

   end Parse_Tuple;

   --  -------------------------------------------------------------------------

   procedure Plan (Classifier    : Python.Module; R : PyObject_Ptr;
                   Pi, Q         :  Real_Float_Matrix;
                   Pi_Out, Q_Out : out Real_Float_Matrix) is
      use Tuple_Builder;
      Routine_Name    : constant String := "Python_21A.Plan  ";

      function Py_BuildValue (Format : char_array; O1, T1, T2 : PyObject_Ptr)
                              return PyObject_Ptr;
      pragma Import (C, Py_BuildValue, "Py_BuildValue");

      F        : constant PyObject_Ptr := Python.Get_Symbol (Classifier, "plan");
      Pi_Tuple : constant PyObject_Ptr := To_Tuple (Pi);
      Q_Tuple  : constant PyObject_Ptr := To_Tuple (Q);
      PyParams : PyObject_Ptr;
      PyResult : PyObject_Ptr;
   begin
      Put_Line (Routine_Name);
      PyParams := Py_BuildValue (To_C ("OOO"), R, Pi_Tuple, Q_Tuple);
      Put_Line (Routine_Name & "Call_Object");
      PyResult := Python.Call_Object (F, PyParams);
      Py_DecRef (F);
      Py_DecRef (Pi_Tuple);
      Py_DecRef (Q_Tuple);
      Py_DecRef (PyParams);
      Put_Line (Routine_Name & "parse");

      Parse_Tuple (PyResult, Pi_Out, Q_Out);
      Py_DecRef (PyResult);
      Put_Line (Routine_Name & "done");

   end Plan;

   --  -------------------------------------------------------------------------

   function To_Tuple (Data : Support_21A.Float_Tensor)  return PyObject_Ptr is
      Routine_Name  : constant String := "Python_21a.To_Tuple ";
      Result        : constant PyObject_Ptr := PyTuple_New (int (Data'Length));
      Py_Tensor     : PyObject_Ptr;
      Py_Index      : int := -1;
   begin
      for mat in Data'Range loop
         Py_Index := Py_Index + 1;
         Py_Tensor := To_Tuple (Data);
         PyTuple_SetItem (Result, Py_Index, Py_Tensor);
      end loop;

      return Result;

   exception
      when E : others =>
         Put_Line (Routine_Name & "error" & Exception_Message (E));
         raise;

   end To_Tuple;

   --  -------------------------------------------------------------------------

end Python_21A;
