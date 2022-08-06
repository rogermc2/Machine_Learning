--  Based on scikit-learn/sklearn/datasets/_base.py
with Ada.Assertions; use Ada.Assertions;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
--  with Ada.Text_IO; use Ada.Text_IO;

with Classifier_Utilities;
--  with Printing;

package body Load_Dataset is

   --  Each Digits datapoint is an 8x8 marix of a digit image.
   --  =================   ==============
   --  Classes                         10
   --  Samples per class             ~180
   --  Samples total                 1797
   --  Dimensionality                  64
   --  Features             integers 0-16
   --  =================   ==============
   --  Target: num outputs x num classes
   function Load_Digits (File_Name : String; Num_Classes : Natural := 10)
                         return Digits_Data_Record is
      use Classifier_Utilities;
      Routine_Name    : constant String := "Load_Dataset.Load_Digits ";
      Digits_Data     : constant NL_Types.Multi_Output_Data_Record :=
                          Load_Data (File_Name);
      Digit_Features  : constant NL_Types.Value_Data_Lists_2D :=
                          Digits_Data.Feature_Values;
      Digit_Values    : constant NL_Types.Value_Data_Lists_2D :=
                          Digits_Data.Label_Values;
      Num_Samples     : constant Positive := Natural (Digit_Features.Length);
      Num_Features    : constant Positive :=
                          Natural (Digit_Features (1).Length);
      List_Row        : NL_Types.Value_Data_List;
      Data            : Digits_Data_Record (Num_Samples, Num_Features,
                                            Num_Classes);
   begin
      Assert (Num_Samples > 0, Routine_Name &
                " called with empty Features vector.");
      Assert (Integer (Digit_Values.Length) = Num_Samples, Routine_Name &
                " invalid Digits Target vector");

      if Num_Classes > 0 then
         for index in 1 .. Num_Classes loop
            Data.Classes (index) := index - 1;
         end loop;
      end if;

      for row in Digit_Features.First_Index .. Digit_Features.Last_Index loop
         List_Row := Digit_Features (row);
         for col in List_Row.First_Index .. List_Row.Last_Index loop
            Data.Features (row, col) := List_Row.Element (col).Integer_Value;
         end loop;
      end loop;

      for row in Digit_Values.First_Index .. Digit_Values.Last_Index loop
         Data.Target (row) :=
           Digit_Values.Element (row).Element (1).Integer_Value;
      end loop;

      if Num_Classes > 0 then
         for index in Data.Target'Range loop
            Data.Target (index) := Data.Target (index) mod Num_Classes;
         end loop;
      end if;

      return Data;

   end Load_Digits;

   --  -------------------------------------------------------------------------

   function Load_Iris (File_Name : String) return Iris_Data_Record is
      use Classifier_Utilities;
      use NL_Types;
      Routine_Name  : constant String := "Load_Dataset.Load_Iris ";
      Iris_Data     : constant NL_Types.Multi_Output_Data_Record :=
                        Load_Data (File_Name);
      Class_Names   : NL_Types.Class_Names_List;
      --        Features        : NL_Types.Feature_Names_List;
      Iris_Features : constant NL_Types.Value_Data_Lists_2D :=
                        Iris_Data.Feature_Values;
      Iris_Labels   : constant NL_Types.Value_Data_Lists_2D :=
                        Iris_Data.Label_Values;
      Iris_Row      : NL_Types.Value_Data_List;
      Num_Samples   : constant Natural := Natural (Iris_Features.Length);
      Data          : Iris_Data_Record;
   begin
      Class_Names.Append (To_Unbounded_String ("Setosa"));
      Class_Names.Append (To_Unbounded_String ("Versicolour"));
      Class_Names.Append (To_Unbounded_String ("Virginica"));

      Assert (Num_Samples > 0, Routine_Name &
                " called with empty Features vector.");
      Assert (Integer (Iris_Data.Label_Values.Length) = Num_Samples, Routine_Name &
                " invalid Iris Target vector");
      Data.Features := To_Float_List_2D (Iris_Data.Feature_Values);
      Data.Num_Features := Positive (Data.Features (1).Length);

      for row in Iris_Labels.First_Index .. Iris_Labels.Last_Index loop
         Iris_Row := Iris_Labels.Element (row);
         if Iris_Row.Element (1).Value_Kind = UB_String_Type then
            if To_String (Iris_Row.Element (1).UB_String_Value) = "Setosa" then
               Data.Target.Append (1);
            elsif To_String (Iris_Row.Element (1).UB_String_Value) =
              "Versicolour" then
               Data.Target.Append (2);
            elsif To_String (Iris_Row.Element (1).UB_String_Value) =
              "Virginica" then
               Data.Target.Append (3);
            else
               Data.Target.Append (0);
            end if;
         else
            Data.Target.Append (0);
         end if;
      end loop;

      return Data;

   end Load_Iris;

   --  -------------------------------------------------------------------------

end Load_Dataset;
