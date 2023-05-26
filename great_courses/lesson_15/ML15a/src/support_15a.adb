
--  with System;

--  with Interfaces.C;

with Ada.Assertions; use Ada.Assertions;
with Ada.Characters.Handling;
with Ada.Strings;
with Ada.Strings.Fixed;
with Ada.Text_IO; use Ada.Text_IO;

--  with Basic_Printing; use Basic_Printing;
with To_BMP;
--  with Python_API;

package body Support_15A is

   procedure Train_Test_Split
     (X                     : Image_Vector; Y : Integer_Array;
      Train_X               : out Image_Vector; Train_Y : out Integer_Array;
      Test_X                : out Image_Vector; Test_Y : out Integer_Array);

   --  -------------------------------------------------------------------------

   --     function Call_Python (M : Python.Module; Function_Name, A : String)
   --                           return Image_Array is
   --        use System;
   --        use Interfaces.C;
   --        use Python;
   --        use Python_API;
   --        Routine_Name : constant String := "Support_15A.Call_Python ";
   --
   --        function Parse_Image_Tuple (Tuple : PyObject) return Image_Array is
   --  --           Routine_Name   : constant String :=
   --  --                              "Support_15A.Call.Parse_Image_Tuple ";
   --           Tuple_Row      : PyObject;
   --           Tuple_Col      : PyObject;
   --           Tuple_RGB      : PyObject;
   --           Image          : Image_Array;
   --        begin
   --           for row in 0 .. 63 loop
   --              Tuple_Row := PyTuple_GetItem (Tuple, int (row));
   --              for col in 0 .. 63 loop
   --                 Tuple_Col := PyTuple_GetItem (Tuple_Row, int (col));
   --                 for rgb in 0 .. 2 loop
   --                    Tuple_RGB := PyTuple_GetItem (Tuple_Col, int (rgb));
   --                    Image (row + 1, col + 1, rgb + 1) :=
   --                      Interfaces.Unsigned_8 (PyInt_AsLong (Tuple_RGB));
   --                 end loop;
   --              end loop;
   --           end loop;
   --
   --           return Image;
   --
   --        end Parse_Image_Tuple;
   --
   --        function Py_BuildValue (Format : Interfaces.C.char_array;
   --                                A      : Interfaces.C.char_array) return PyObject;
   --        pragma Import (C, Py_BuildValue, "Py_BuildValue");
   --        F        : constant PyObject := Get_Symbol (M, Function_Name);
   --        PyParams : PyObject;
   --        PyResult : PyObject;
   --     begin
   --        PyParams := Py_BuildValue (To_C ("(s)"), To_C (A));
   --        PyResult := Call_Object (F, PyParams);
   --
   --        Py_DecRef (F);
   --        Py_DecRef (PyParams);
   --
   --        Assert (PyResult /= Null_Address, Routine_Name & "PyResult is null");
   --
   --        declare
   --           Image : constant Image_Array := Parse_Image_Tuple (PyResult);
   --        begin
   --           Py_DecRef (PyResult);
   --           return Image;
   --        end;
   --
   --     end Call_Python;

   --  -------------------------------------------------------------------------

   function Get_Picture (File_Name : String; Show_Name : Boolean := True)
                         return Unsigned_8_Array_3D is
      use Ada.Characters.Handling;
      Routine_Name    : constant String := "Support_5A.Get_Picture ";
      File_Name_Upper : constant String := To_Upper (File_Name);
      File_Kind       : constant String :=
                          File_Name_Upper
                            (File_Name_Upper'Last - 4 .. File_Name_Upper'Last);
      --        Swap            : Interfaces.Unsigned_8;
   begin
      if File_Kind = ".PNG" then
         declare
            Initial : constant Unsigned_8_Array_3D :=
                        Unsigned_8_Array_3D (To_BMP.Process (File_Name));
            Clipped : Unsigned_8_Array_3D
              (1 .. Initial'Length - 15, 1 .. Initial'Length (2) - 1,
               Initial'Range (3));
         begin
            for row in Clipped'Range loop
               for col in Clipped'Range (2) loop
                  for pix in Clipped'Range (3) loop
                     Clipped (row, col, pix) := Initial (row, col, pix);
                  end loop;
                  --                    Swap := Clipped (row, col, 2);
                  --                    Clipped (row, col, 2) := Clipped (row, col, 3);
                  --                    Clipped (row, col, 3) := Swap;
               end loop;
            end loop;

            return Clipped;
         end;

      elsif File_Kind = ".JPG" then
         return Unsigned_8_Array_3D (To_BMP.Process (File_Name, Show_Name));

      elsif File_Kind = ".JPEG" then
         return Unsigned_8_Array_3D (To_BMP.Process (File_Name, Show_Name));

      else
         Put_Line (Routine_Name & "unsupported image format " & File_Kind);
         declare
            Dummy : constant Unsigned_8_Array_3D (1 .. 1, 1 .. 1, 1 .. 1) :=
                      (1 => (1 => (1 => 0)));
         begin
            return Dummy;
         end;
      end if;

   end Get_Picture;

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

   --     procedure Read_Cats (M                   : Python.Module;
   procedure Read_Cats (Cats_Dir            : String_9_Array;
                        Label               : Natural;
                        Num_Samples         : Positive;
                        Train_X, Test_X     : out Image_Vector;
                        Train_Y, Test_Y     : out Integer_Array) is
      use Ada.Strings;
      use Ada.Strings.Fixed;
      Routine_Name    : constant String := "Support_15A.Read_Cats ";
      --        Train_Size      : constant Positive := Train_X'Length;
      --        Test_Size       : constant Positive := Test_X'Length;
      Train_Directory : constant String :=
                          "../../../../imgs/tiny-imagenet-200/train/";
      Images          : Image_Vector (1 .. Cats_Dir'Length * Num_Samples);
      Labels          : Integer_Array (1 .. Cats_Dir'Length * Num_Samples);
      Image_File_Dir  : String_9;
   begin
      Put_Line (Routine_Name & "reading training files.");
      for cat in Cats_Dir'Range loop
         Image_File_Dir := Cats_Dir (cat);
--           Put_Line (Routine_Name & "reading " & String (Image_File_Dir));
         for img in 0 .. Num_Samples - 1 loop
            --              Put_Line (Routine_Name & "reading image " &
            --                          Integer'Image (cat + img));
            declare
               Image_Data  : constant Unsigned_8_Array_3D :=
                               Get_Picture (Train_Directory &
                                                          String (Image_File_Dir) & "/images/" &
                                              String (Image_File_Dir) & "_" &
                                              Trim (Integer'Image (img), Both) & ".JPEG",
                                            False);
            begin
               null;
            end;
            --              Images (cat + img) :=
            --                Call_Python (M, "load_image", Train_Directory &
            --                               String (Image_File_Dir) & "/images/" &
            --                               String (Image_File_Dir) & "_" &
            --                               Trim (Integer'Image (img), Both) & ".JPEG");
            Labels (cat + img) := Label;
         end loop;
      end loop;
      Put_Line (Routine_Name & "training files read.");

      Train_Test_Split (Images, Labels, Train_X, Train_Y, Test_X, Test_Y);

   end Read_Cats;

   --  -------------------------------------------------------------------------

   procedure Train_Test_Split
     (X                     : Image_Vector; Y : Integer_Array;
      Train_X               : out Image_Vector; Train_Y : out Integer_Array;
      Test_X                : out Image_Vector; Test_Y : out Integer_Array) is
      Routine_Name : constant String := "Support_15A.Train_Test_Split ";
      Num_Samples  : constant Positive := X'Length;
      Train_Size   : constant Positive := Train_X'Length;
      Test_Size    : constant Positive := Test_X'Length;
      Image_In     : Image_Array;
      Image_Out    : Image_Array;
   begin
      Assert (Natural (Y'Length) = Num_Samples, Routine_Name &
                "Y length" & Integer'Image (Integer (Y'Length)) &
                " is different to X length" & Natural'Image (Num_Samples));

      for img in Train_X'Range loop
         Image_In := X (img);
         for row in Image_In'Range loop
            for col in Image_In'Range (2) loop
               for rgb in Image_In'Range (3) loop
                  Image_Out (row, col, rgb) := Image_In (row, col, rgb);
               end loop;
            end loop;
         end loop;
         Train_X (img) := Image_Out;
         Train_Y (img) := Y (img);
      end loop;

      for img in 1 .. Test_X'Length loop
         Image_In := X (Train_Size + img);
         for row in Image_In'Range loop
            for col in Image_In'Range (2) loop
               for rgb in Image_In'Range (3) loop
                  Image_Out (row, col, rgb) :=
                    Image_In (row, col, rgb);
               end loop;
            end loop;
         end loop;
         Test_X (img) := Image_Out;
         Test_Y (img) := Y (Train_Size + img);
      end loop;

   end Train_Test_Split;

   --  -------------------------------------------------------------------------

end Support_15A;
