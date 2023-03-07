--  Based on scikit-learn/sklearn/datasets/_base.py

with Ada.Assertions; use Ada.Assertions;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;

--  with Basic_Printing; use Basic_Printing;
with Classifier_Loader;
with Neural_Loader;
with Type_Utilities;

package body Load_Dataset is

   function Load_Diabetes (File_Name : String) return Diabetes_Data_Record is
      use Classifier_Loader;
      use ML_Types;
      Routine_Name      : constant String := "Load_Dataset.Load_Diabetes ";
      Diabetes_Data     : constant ML_Types.Multi_Output_Data_Record :=
                            Load_Data (File_Name);
      Diabetes_Features : constant Value_Data_Lists_2D :=
                            Diabetes_Data.Feature_Values;
      Diabetes_Labels   : constant Value_Data_Lists_2D :=
                            Diabetes_Data.Label_Values;
      Num_Samples       : constant Natural :=
                            Natural (Diabetes_Features.Length);
      Diabetes_Row      : Value_Data_List;
      New_Row           : NL_Types.Float_List;
      Data              : Diabetes_Data_Record;
   begin
      Assert (Num_Samples > 0, Routine_Name &
                " called with empty Features vector.");
      Assert (Integer (Diabetes_Data.Label_Values.Length) = Num_Samples,
              Routine_Name & " invalid Diabetes Target vector");
      Data.Feature_Names := Diabetes_Data.Feature_Names;

      for row in Diabetes_Data.Feature_Values.First_Index ..
        Diabetes_Data.Feature_Values.Last_Index loop
         Diabetes_Row := Diabetes_Data.Feature_Values.Element (row);
         New_Row.Clear;
         for col in Diabetes_Row.First_Index .. Diabetes_Row.Last_Index loop
            if Diabetes_Row.Element (col).Value_Kind = Integer_Type then
               New_Row.Append
                 (Float (Diabetes_Row.Element (col).Integer_Value));
            else
               New_Row.Append (Diabetes_Row.Element (col).Float_Value);
            end if;
         end loop;
         Data.Features.Append (New_Row);
      end loop;

      for row in Diabetes_Data.Label_Values.First_Index ..
        Diabetes_Data.Label_Values.Last_Index loop
         Diabetes_Row := Diabetes_Labels.Element (row);
         Data.Target.Append (Diabetes_Row.Element (1).Integer_Value);
      end loop;

      return Data;

   end Load_Diabetes;

   --  -------------------------------------------------------------------------

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
      use Classifier_Loader;
      use Type_Utilities;
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
            Put_Line (Routine_Name & "Num_Classes:" &
                        Integer'Image (Num_Classes));
            Put_Line (Routine_Name & "Index_Values Length:" &
                        Integer'Image (Index_Values'Length));
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
                    Neural_Loader.Load_Raw_CSV_Data (File_Name);
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
      use Ada.Strings;
      use Classifier_Loader;
      use Type_Utilities;
      use ML_Types;
      Routine_Name  : constant String := "Load_Dataset.Load_Iris ";
      Iris_Data     : constant ML_Types.Multi_Output_Data_Record :=
                        Load_Data (File_Name);
      Class_Names   : Class_Names_List;
      --        Feature_Names : String_List := Iris_Data.Feature_Names;
      --        Label_Names   : ML_Types.Unbounded_List := Iris_Data.Label_Names;
      Iris_Features : constant Value_Data_Lists_2D :=
                        Iris_Data.Feature_Values;
      Iris_Labels   : constant Value_Data_Lists_2D :=
                        Iris_Data.Label_Values;
      Num_Samples   : constant Natural := Natural (Iris_Features.Length);
      Iris_Row      : Value_Data_List;
      Data          : Iris_Data_Record;
      Species       : Unbounded_String;
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
            Species := Trim (Iris_Row.Element (1).UB_String_Value, Both);
            if To_String (Species) = "setosa" then
               Data.Target.Append (1);
            elsif To_String (Species) = "versicolor" then
               Data.Target.Append (2);
            elsif To_String (Species) = "virginica" then
               Data.Target.Append (3);
            else
               Assert (False, Routine_Name & "row" & Integer'Image (row) &
                         ": invalid species '" & To_String (Species) & "'");
            end if;
         else
            Assert (False, Routine_Name & "row" & Integer'Image (row) &
                      ": invalid species '" & To_String (Species) & "'");
         end if;
      end loop;

      return Data;

   end Load_Iris;

   --  -------------------------------------------------------------------------

   function Load_Labels (File_Name : String; Num_Outputs : Positive := 1)
                         return Integer_Matrix is
      use ML_Types;
      CSV_Data   : constant Raw_Data_Vector :=
                     Neural_Loader.Load_Raw_CSV_Data (File_Name);
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
