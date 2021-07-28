
with Ada.Strings.Fixed;
with Ada.Text_IO; use Ada.Text_IO;

with Utilities;

with ML_Types;

with Classifier_Utilities;
with Tree;
with Decision_Tree_Classifer;
with Estimator;

procedure Lesson_3 is
   use Ada.Strings;
   use Utilities;
   use ML_Types;
   use Decision_Tree_Classifer;

   Data_File    : File_Type;
begin
   Put_Line ("Lesson 3");
   Open (Data_File, In_File, "src/diabetes.csv");
   declare
      Line_1       : constant String := Get_Line (Data_File);
      Num_Features : constant Integer := Fixed.Count (Line_1, ",");
      Row_Data     : Rows_Vector;
      Data         : Data_Record;
      Weights      : constant Classifier_Utilities.Float_Array (1 .. Num_Features) :=
                       (others => 0.0);
   begin
      Put_Line ("Feature Names:");
      Utilities.Load_CSV_Data (Data_File, Row_Data);
      Data := Split_Row_Data (Row_Data);
      declare
         aClassifier : Classifier
           (Tree.Integer_Type, Tree.Integer_Type, Tree.Integer_Type);
         Estimate    : Estimator.Estimator_Data
           (Positive (Row_Data.Length), Num_Features + 1);
      begin
         --  Fit function adjusts weights according to data values so that
         --  better accuracy can be achieved
         Estimate := Decision_Tree_Classifer.Fit
           (aClassifier, Data.Feature_Values, Data.Label_Values, Weights);
      end;
   end;

   Close (Data_File);

end Lesson_3;
