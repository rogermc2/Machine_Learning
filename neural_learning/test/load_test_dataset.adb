--  Based on scikit-learn/sklearn/datasets/_base.py
with Ada.Assertions; use Ada.Assertions;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
--  with Ada.Text_IO; use Ada.Text_IO;

with Classifier_Utilities;
--  with Printing;

package body Load_Test_Dataset is

   --  Each Digits datapoint is an 8x8 marix of a digit image.
   --  =================   ==============
   --  Classes                         10
   --  Samples per class             ~180
   --  Samples total                 1797
   --  Dimensionality                  64
   --  Features             integers 0-16
   --  =================   ==============
   --  Target: num outputs x num classes
   function Load_Digits return Data_Record is
      use Classifier_Utilities;
      Routine_Name   : constant String := "Load_Test_Dataset.Load_Digits ";
      Digits_Data    : constant NL_Types.Multi_Output_Data_Record :=
                         Load_Data ("../../digits.csv");
      Class_Names    : NL_Types.Class_Names_List;
      --          Feature_Names       : NL_Types.Feature_Names_List;
      Digit_Features : constant NL_Types.Value_Data_Lists_2D :=
                         Digits_Data.Feature_Values;
      Digit_Labels   : constant NL_Types.Value_Data_List :=
                         Digits_Data.Feature_Values.Element (1);
      Num_Samples    : constant Natural := Natural (Digit_Features.Length);
      Data           : Data_Record;
   begin
      Class_Names.Append (To_Unbounded_String ("0"));
      Class_Names.Append (To_Unbounded_String ("1"));
      Class_Names.Append (To_Unbounded_String ("2"));
      Class_Names.Append (To_Unbounded_String ("3"));
      Class_Names.Append (To_Unbounded_String ("4"));
      Class_Names.Append (To_Unbounded_String ("5"));
      Class_Names.Append (To_Unbounded_String ("6"));
      Class_Names.Append (To_Unbounded_String ("7"));
      Class_Names.Append (To_Unbounded_String ("8"));
      Class_Names.Append (To_Unbounded_String ("9"));
      Class_Names.Append (To_Unbounded_String ("10"));

      Assert (Num_Samples > 0, Routine_Name &
                " called with empty Features vector.");
      Assert (Integer (Digits_Data.Label_Values.Length) = Num_Samples, Routine_Name &
                " invalid Digits Target vector");

      --  Digits_Target is 2D list num outputs x num classes
      Data.Features := To_Float_List_2D (Digit_Features);
      Data.Target := To_Integer_List (Digit_Labels);
      Data.Num_Features := Positive (Data.Features (1).Length);
      return Data;

   end Load_Digits;

   --  -------------------------------------------------------------------------

   function Load_Iris return Data_Record is
      use Classifier_Utilities;
      use NL_Types;
      Routine_Name  : constant String := "Load_Test_Dataset.Load_Iris ";
      Iris_Data     : constant NL_Types.Multi_Output_Data_Record :=
                        Load_Data ("../../iris.csv");
      Class_Names   : NL_Types.Class_Names_List;
      --        Features        : NL_Types.Feature_Names_List;
      Iris_Features : constant NL_Types.Value_Data_Lists_2D :=
                        Iris_Data.Feature_Values;
      Iris_Labels   : constant NL_Types.Value_Data_Lists_2D :=
                        Iris_Data.Label_Values;
      Iris_Row      : NL_Types.Value_Data_List;
      Num_Samples   : constant Natural := Natural (Iris_Features.Length);
      Data          : Data_Record;
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

end Load_Test_Dataset;
