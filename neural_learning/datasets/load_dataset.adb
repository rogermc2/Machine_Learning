--  Based on scikit-learn/sklearn/datasets/_base.py
with Ada.Assertions; use Ada.Assertions;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;

with Classifier_Utilities;
--  with Printing;
with Neural_Utilities;

package body Load_Dataset is

   --  Each Digits datapoint is an 8x8 matrix of a digit image.
   --  =================   ==============
   --  Classes                         10
   --  Samples per class             ~180
   --  Samples total                 1797
   --  Dimensionality                  64
   --  Features             integers 0-16
   --  =================   ==============
   --  Target: num outputs x num classes
   function Load_Digits (File_Name : String; Num_Classes : Natural := 10;
                         Max_Lines : Positive := 20000)
                         return Digits_Data_Record is
      use Classifier_Utilities;
      Routine_Name    : constant String := "Load_Dataset.Load_Digits ";
      Digit_Features  : ML_Types.Value_Data_Lists_2D;
      Digit_Values    : ML_Types.Value_Data_Lists_2D;
   begin
      Put_Line (Routine_Name & "loading " & File_Name);
      declare
         Digits_Data : constant ML_Types.Multi_Output_Data_Record :=
                         Load_Data (File_Name, Max_Lines => Max_Lines);
      begin
         Digit_Features := Digits_Data.Feature_Values;
         Digit_Values := Digits_Data.Label_Values;
      end;
      Put_Line (Routine_Name & File_Name & " loaded");

      declare
         Num_Samples     : constant Positive := Natural (Digit_Features.Length);
         Num_Features    : constant Positive :=
                             Natural (Digit_Features (1).Length);
         Index_Values    : Boolean_Array (1 .. Integer (Digit_Values.Length));
         Short_Features  : ML_Types.Integer_List_2D;
         Short_Values    : ML_Types.Integer_List;
         List_Row        : ML_Types.Integer_List;
         Features_Row    : ML_Types.Integer_List;
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

         if Num_Classes < 10 then
            for index in Index_Values'Range loop
               List_Row := To_Integer_List (Digit_Values (index));
               Index_Values (index) := List_Row (1) < Num_Classes;
            end loop;

            for row in Index_Values'Range loop
               if Index_Values (row) then
                  Short_Values.Append (To_Integer_List (Digit_Values (row)));
                  Features_Row := To_Integer_List (Digit_Features (row));
                  Short_Features.Append (Features_Row);
               end if;
            end loop;

            declare
               Short_Data : Digits_Data_Record
                 (Integer (Short_Values.Length), Num_Features, Num_Classes);
            begin
               Short_Data.Target := To_Integer_Array (Short_Values);
               Short_Data.Features := To_Integer_Matrix (Short_Features);
               Put_Line (Routine_Name & File_Name & " loaded");
               return Short_Data;
            end;

         else
            for row in Digit_Features.First_Index .. Digit_Features.Last_Index loop
               List_Row := To_Integer_List (Digit_Features (row));
               for col in List_Row.First_Index .. List_Row.Last_Index loop
                  Data.Features (row, col) := List_Row.Element (col);
               end loop;
            end loop;
            Put_Line (File_Name & " features loaded");

            for row in Digit_Values.First_Index .. Digit_Values.Last_Index loop
               Data.Target (row) :=
                 Digit_Values.Element (row).Element (1).Integer_Value;
            end loop;

            return Data;
         end if;
      end;  -- declare block

   end Load_Digits;

   --  -------------------------------------------------------------------------

   function Load_Features (File_Name : String; Num_Features : Positive)
                           return Real_Float_Matrix is
      CSV_Data  : constant ML_Types.Raw_Data_Vector :=
                    Neural_Utilities.Load_Raw_CSV_Data (File_Name);
      List_Row  : ML_Types.Unbounded_List;
      Features  : ML_Arrays_And_Matrices.Real_Float_Matrix
        (1 .. Positive (CSV_Data.Length) - 1, 1 .. Num_Features);
   begin
    Put_Line ("Loading " & File_Name & " Features");
      --  First row of CSV_Data is header of feature names
      for row in Features'Range loop
         List_Row := CSV_Data (row + 1);
         for col in Features'Range( 2) loop
            Features (row, col) := Float'Value (To_String (List_Row (col)));
         end loop;
      end loop;

      return Features;

   end Load_Features;

   --  -------------------------------------------------------------------------

   function Load_Iris (File_Name : String) return Iris_Data_Record is
      use Classifier_Utilities;
      use ML_Types;
      Routine_Name  : constant String := "Load_Dataset.Load_Iris ";
      Iris_Data     : constant ML_Types.Multi_Output_Data_Record :=
                        Load_Data (File_Name);
      Class_Names   : Class_Names_List;
      --        Features        : NL_Types.Feature_Names_List;
      Iris_Features : constant Value_Data_Lists_2D :=
                        Iris_Data.Feature_Values;
      Iris_Labels   : constant Value_Data_Lists_2D :=
                        Iris_Data.Label_Values;
      Iris_Row      : Value_Data_List;
      Num_Samples   : constant Natural := Natural (Iris_Features.Length);
      Data          : Iris_Data_Record;
   begin
      Class_Names.Append (To_Unbounded_String ("Setosa"));
      Class_Names.Append (To_Unbounded_String ("Versicolour"));
      Class_Names.Append (To_Unbounded_String ("Virginica"));

      Assert (Num_Samples > 0, Routine_Name &
                " called with empty Features vector.");
      Assert (Integer (Iris_Data.Label_Values.Length) = Num_Samples,
              Routine_Name & " invalid Iris Target vector");
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

   function Load_Labels (File_Name : String; Num_Outputs : Positive := 1)
                         return Integer_Matrix is
      use ML_Types;
      CSV_Data   : constant Raw_Data_Vector :=
                     Neural_Utilities.Load_Raw_CSV_Data (File_Name);
      Data_Row   : Unbounded_List;
      Labels     : Integer_Matrix (1 .. Positive (CSV_Data.Length) - 1,
                                   1 .. Num_Outputs);
   begin
    Put_Line ("Loading " & File_Name & " Labels");
      --  First row is header of label names
      for row in Labels'Range loop
         Data_Row := CSV_Data.Element (row + 1);
         for col in Labels'Range (2) loop
            Labels (row, col) :=
              Integer'Value (To_String (Data_Row.Element (col)));
         end loop;
      end loop;

      return Labels;

   end Load_Labels;

   --  -------------------------------------------------------------------------

end Load_Dataset;
