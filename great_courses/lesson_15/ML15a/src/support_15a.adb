
with Interfaces.C;

with Ada.Assertions; use Ada.Assertions;
with Ada.Text_IO; use Ada.Text_IO;

--  with Basic_Printing; use Basic_Printing;
with Python_API;

package body Support_15A is

   procedure Train_Test_Split
     (X          : Image_Vector; Y : Integer_Array;
      Train_Size : Float; Test_Size : Float;
      Train_X    : out Image_Vector; Train_Y : out Integer_Array;
      Test_X     : out Image_Vector; Test_Y : out Integer_Array);

   --  -------------------------------------------------------------------------

   function Call (M : Python.Module; Function_Name, A : String)
                  return Image_Array is
      use Python;
      use Python_API;

      function Parse_Tuple (Tuple : PyObject) return Image_Array is
         use Interfaces.C;
         --        Routine_Name : constant String := "Support_15A.Parse_Tuple ";
         Tuple_Row      : PyObject;
         Tuple_Col      : PyObject;
         Tuple_RGB      : PyObject;
         Image          : Image_Array;
      begin
         for row in 0 .. 63 loop
            Tuple_Row := PyTuple_GetItem (Tuple, int (row));
            for col in 0 .. 63 loop
               Tuple_Col := PyTuple_GetItem (Tuple_Row, int (col));
               for rgb in 0 .. 2 loop
                  Tuple_RGB := PyTuple_GetItem (Tuple_Col, int (rgb));
                  Image (row + 1, col + 1, rgb + 1) :=
                    Interfaces.Unsigned_8 (PyInt_AsLong (Tuple_RGB));
               end loop;
            end loop;
         end loop;

         return Image;

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
         Image : constant Image_Array := Parse_Tuple (PyResult);
      begin
         Py_DecRef (PyResult);
         return Image;
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

   procedure Read_Cats (M                     : Python.Module;
                        Cats                  : String_9_Array;
                        Label                 : Natural;
                        Train_Size, Test_Size : Float;
                        Train_X, Test_X     : out Image_Vector;
                        Train_Y, Test_Y     : out Integer_Array) is
      Routine_Name    : constant String := "Support_15A.Read_Cats ";
      Train_Directory : constant String :=
                          "../../imgs/tiny-imagenet-200/train/";
      Num_Samples     :constant  Positive := 500;
      Images          : Image_Vector (1 .. Num_Samples);
      Labels          : Integer_Array (1 .. Num_Samples);
      Image_File_Dir  : String_9;
   begin
      Put_Line (Routine_Name & "reading training files.");
      for cat in Cats'Range loop
         Image_File_Dir := Cats (cat);
         for index in 0 .. Num_Samples - 1 loop
            Images (cat + index) :=
              Call (M, "load_image", Train_Directory &
                      String (Image_File_Dir) & "images" &
                      String (Image_File_Dir) & "_" & Integer'Image (index) &
                      ".JPEG");
            Labels (cat + index) := Label;
         end loop;
      end loop;
      Put_Line (Routine_Name & "training files read.");

      Train_Test_Split (Images, Labels, Train_Size, Test_Size,
                        Train_X, Train_Y, Test_X, Test_Y);

   end Read_Cats;

   --  -------------------------------------------------------------------------

   procedure Train_Test_Split
     (X          : Image_Vector; Y : Integer_Array;
      Train_Size : Float; Test_Size : Float;
      Train_X    : out Image_Vector; Train_Y : out Integer_Array;
      Test_X     : out Image_Vector; Test_Y : out Integer_Array) is
      Routine_Name : constant String := "Support_15A.Train_Test_Split ";
      Num_Samples  : constant Positive := X'Length;
      Num_Train    : constant Positive :=
                       Positive (Train_Size * Float (Num_Samples));
      Num_Test     : constant Positive :=
                       Positive (Test_Size * Float (Num_Samples));
      Image_In     : Image_Array;
      Image_Out    : Image_Array;
   begin
      Assert (Natural (Y'Length) = Num_Samples, Routine_Name &
                "Y length" & Integer'Image (Integer (Y'Length)) &
                " is different to X length" & Natural'Image (Num_Samples));

      for img in 1 .. Num_Train loop
         Image_In := X (img);
         for row in Image_In'Range loop
            for col in Image_In'Range (2) loop
               for rgb in Image_In'Range (3) loop
                  Image_Out (row, col, rgb) := Image_In (row, col, rgb);
               end loop;
               Train_Y (row) := Y (row);
            end loop;
         end loop;
         Train_X (img) := Image_Out;
      end loop;

      for img in 0 .. Num_Test - 1 loop
         Image_In := X (Num_Train + img);
         for row in Image_In'Range loop
            for col in Image_In'Range (2) loop
               for rgb in Image_In'Range (3) loop
                  Image_Out (row, col, rgb) := Image_In (row + Num_Train, col, rgb);
               end loop;
               Test_Y (row) := Y (row + Num_Train);
            end loop;
         end loop;
         Test_X (img) := Image_Out;
      end loop;

   end Train_Test_Split;

   --  -------------------------------------------------------------------------

end Support_15A;
