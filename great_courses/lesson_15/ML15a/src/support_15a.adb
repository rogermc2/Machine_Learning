
with Interfaces;
with Interfaces.C;

with Ada.Assertions; use Ada.Assertions;
--  with Ada.Text_IO; use Ada.Text_IO;

--  with Basic_Printing; use Basic_Printing;
with Python_API;

package body Support_15A is

   type Image_Array is array (Integer range 1 .. 64, Integer range 1 .. 64,
                              Integer range 1 .. 3) of Interfaces.Unsigned_8;
   type Image_Vector is array (Integer range <>) of Image_Array;

   --  -------------------------------------------------------------------------

   function Call (M : Python.Module; Function_Name, A : String)
                  return Image_Vector is
      use Python;
      use Python_API;

      function Parse_Tuple (Tuple : PyObject) return Image_Vector is
         use Interfaces.C;
         --        Routine_Name : constant String := "Support_15A.Parse_Tuple ";
         Tuple_Size     : constant int := PyTuple_Size (Tuple);
         Tuple_Image    : PyObject;
         Tuple_Row      : PyObject;
         Tuple_Col      : PyObject;
         Tuple_RGB      : PyObject;
         Image          : Image_Array;
         Result         : Image_Vector (1 .. Integer (Tuple_Size));
      begin
         for img in 0 .. Tuple_Size - 1 loop
            Tuple_Image := PyTuple_GetItem (Tuple, img);
            for row in 0 .. 63 loop
               Tuple_Row := PyTuple_GetItem (Tuple_Image, int (row));
               for col in 0 .. 63 loop
                  Tuple_Col := PyTuple_GetItem (Tuple_Row, int (col));
                  for rgb in 0 .. 2 loop
                     Tuple_RGB := PyTuple_GetItem (Tuple_Col, int (rgb));
                     Image (row + 1, col + 1, rgb + 1) :=
                       Interfaces.Unsigned_8 (PyInt_AsLong (Tuple_RGB));
                  end loop;
               end loop;
            end loop;
            Result (Integer (img + 1)) := Image;
         end loop;

         return Result;

      end Parse_Tuple;

      --  -------------------------------------------------------------------------

      function Py_BuildValue (Format : Interfaces.C.char_array;
                              A      : Interfaces.C.char_array) return PyObject;
      pragma Import (C, Py_BuildValue, "Py_BuildValue");

      F        : constant PyObject := Get_Symbol (M, Function_Name);
      PyParams : PyObject;
      PyResult : PyObject;
   begin
      PyParams :=
        Py_BuildValue (Interfaces.C.To_C ("(s)"),
                       Interfaces.C.To_C (A));

      PyResult := Call_Object (F, PyParams);
      Py_DecRef (F);
      Py_DecRef (PyParams);

      declare
         Result : constant Image_Vector := Parse_Tuple (PyResult);
      begin
         Py_DecRef (PyResult);
         return Result;
      end;

   end Call;

   --  -------------------------------------------------------------------------

   function Max (Values : Real_Float_Vector) return Float is
      Max_Value : Float := Values (Values'First);
   begin
      for row in Values'Range loop
         if Values (row) > Max_Value then
            Max_Value := Values (row);
         end if;
      end loop;

      return Max_Value;

   end Max;

   --  -------------------------------------------------------------------------

   function Read_Cats (M                     : Python.Module; Cats : String_9_Array; Labels : Labels_Array;
                       Train_Size, Test_Size : Positive) return Boolean is
      Image_Directory : constant String :=
                          "../../great_courses_ml/imgs/tiny-imagenet-200/train/images/";
      Num_Samples     :constant  Positive := 500;
      Images          : Image_Vector (1 .. Num_Samples);
      Image_File_Dir  : String_9;
   begin
      for cat in Images'Range loop
         Images := Call (M, "load_image",
                         Image_Directory & String (Image_File_Dir));
      end loop;

      return False;

   end Read_Cats;

   --  -------------------------------------------------------------------------

   procedure Train_Test_Split
     (X          : Real_Float_Matrix; Y : Integer_Array;
      Train_Size : Natural; Test_Size : Natural;
      Train_X    : out Real_Float_Matrix; Train_Y : out Integer_Array;
      Test_X     : out Real_Float_Matrix; Test_Y : out Integer_Array) is
      Routine_Name : constant String := "CSV_Data_Loader.Train_Test_Split ";
      Num_Samples  : constant Positive := X'Length;
   begin
      Assert (Natural (Y'Length) = Num_Samples, Routine_Name &
                "Y length" & Integer'Image (Integer (Y'Length)) &
                " is different to X length" & Natural'Image (Num_Samples));

      for row in 1 .. Train_Size loop
         for col in X'Range (2) loop
            Train_X (row, col) := X (row, col);
         end loop;
         Train_Y (row) := Y (row);
      end loop;

      for row in 1 .. Test_Size loop
         for col in X'Range (2) loop
            Test_X (row, col) := X (row + Train_Size, col);
         end loop;
         Test_Y (row) := Y (row + Train_Size);
      end loop;

   end Train_Test_Split;

   --  -------------------------------------------------------------------------

end Support_15A;
