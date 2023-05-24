
with Interfaces;

with Ada.Assertions; use Ada.Assertions;
--  with Ada.Text_IO; use Ada.Text_IO;

with Maths;

--  with Basic_Printing; use Basic_Printing;
with Python_Class;

package body Support_15A is

   function Action_Picker (Classifier  : Python.Module;
                           Env         : Python_API.PyObject;
                           CLF         : Python_Class.PyClass :=
                             System.Null_Address;
                           Observation : Integer_Array;
                           Epsilon     : Float) return Boolean is
      use System;
      Routine_Name : constant String := "Support_16a.Action_Picker ";
      Examples     : Integer_Matrix (1 .. 2, Observation'Range);
      Action       : Boolean;
   begin
      if CLF = Null_Address then
         Action := Python.Call (Classifier, "sample", Env);
      else
         Assert (CLF /= Null_Address, Routine_Name & "CLF is null!");

         for col in Observation'Range loop
            Examples (1, col) := Observation (col);
            Examples (2, col) := Observation (col);
         end loop;
         Examples (1, Examples'Last (2)) := 0;
         Examples (2, Examples'Last (2)) := 1;

         declare
            Predictions : constant Integer_Array :=
              Python_Class.Call (Classifier, "predict", Clf,
                                 Examples);
         begin
            Action := Predictions (2) > Predictions (1);
         end;
      end if;

      --  Random_Float range 0.0 .. 1.0
      if Maths.Random_Float < Epsilon then
         Action := Python.Call (Classifier, "sample", Env);
      end if;

      return Action;

   end Action_Picker;

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

   function Read_Cats (M : Python.Module; Cats : String_9_Array; Labels : Labels_Array;
                       Train_Size, Test_Size : Positive) return Boolean is
      type Image_Array is array (Integer range 1 .. 64, Integer range 1 .. 64,
                                 Integer range 1 .. 3) of Interfaces.Unsigned_8;
      type Image_Vector is array (Integer range <>) of Image_Array;
      Image_Directory : constant String :=
        "../../great_courses_ml/imgs/tiny-imagenet-200/train/images/";
      Num_Samples    :constant  Positive := 500;
      Images         : Image_Vector (1 .. Num_Samples);
      Image_File_Dir : String_9;
   begin
      for cat in Images'Range loop
         Python.Call (M, "load_image", Image_Directory & String (Image_File_Dir));
      end loop;

      return False;

   end Read_Cats;

   --  -------------------------------------------------------------------------

   procedure Train_Test_Split
     (X : Real_Float_Matrix; Y : Integer_Array;
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
