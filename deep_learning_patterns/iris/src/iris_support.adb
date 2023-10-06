
--  with Ada.Assertions; use Ada.Assertions;
--  with Ada.Text_IO; use Ada.Text_IO;

with Maths;

--  with Basic_Printing; use Basic_Printing;
with Classifier_Loader;
with ML_Types;
with NL_Types;
with Shuffler;
with Type_Utilities;

package body Iris_Support is

   function Means (M : Real_Float_Matrix) return Real_Float_Vector;
   function Standard_Deviation (M : Real_Float_Matrix)
                                return Real_Float_Vector;
   --  -------------------------------------------------------------------------

   function Build_Dataset return Dataset is
      use NL_Types.Float_Package;
      use NL_Types.Float_List_Package;
      use Type_Utilities;
      --        Routine_Name      : constant String :=
      --                              "Iris_Support.Build_Dataset ";
      Train_Length      : constant Positive := 70;
      Test_Length       : constant Positive := 30;
      Iris_Data         : constant ML_Types.Multi_Output_Data_Record :=
                            Classifier_Loader.Load_Data ("src/iris_01.csv");
      Features          : constant NL_Types.Float_List_2D :=
                            To_Float_List_2D (Iris_Data.Feature_Values);
      Target            : constant NL_Types.Float_List_2D :=
                            To_Float_List_2D
                              (To_Integer_List_2D (Iris_Data.Label_Values));
      Target_Item       : NL_Types.Float_List;
      Feature_Row       : NL_Types.Float_List;
      X                 : Real_Float_Matrix
        (1 .. Positive (Features.Length), 1 .. 4);
      X_Means           : Real_Float_Vector (X'Range (2));
      X_SDs             : Real_Float_Vector (X'Range (2));
      I0                : ML_Types.Integer_List;
      I1                : ML_Types.Integer_List;
      theDataset        : Dataset (Train_Length, Test_Length, 4);
   begin
      for row in X'Range loop
         Feature_Row := Features (row);
         X (row, 1) := Feature_Row (1);
         X (row, 2) := Feature_Row (2);
         X (row, 3) := Feature_Row (3);
         X (row, 4) := Feature_Row (4);
      end loop;

      X_Means := Means (X);
      X_SDs := Standard_Deviation (X);
      for row in X'Range loop
         Feature_Row := Features (row);
         X (row, 1) := (X (row, 1) - X_Means (1)) / X_SDs (1);
         X (row, 2) := (X (row, 2) - X_Means (2)) / X_SDs (2);
         X (row, 3) := (X (row, 3) - X_Means (3)) / X_SDs (3);
         X (row, 4) := (X (row, 4) - X_Means (4)) / X_SDs (4);
      end loop;

      for index in Target.First_Index .. Target.Last_Index loop
         Target_Item := Target (index);
         if Target_Item.First_Element = 0.0 then
            I0.Append (index);
         elsif Target_Item.First_Element = 1.0 then
            I1.Append (index);
         end if;
      end loop;

      declare
         I0_Length : constant Natural := Integer (I0.Length);
         I1_Length : constant Natural := Integer (I1.Length);
         X_Trimmed : Real_Float_Matrix
           (1 .. I0_Length + I1_Length, 1 .. 4);
      begin
         for row in I0.First_Index .. I0.Last_Index loop
            X_Trimmed (row + 1, 1) := X (I0 (row), 1);
            X_Trimmed (row + 1, 2) := X (I0 (row), 2);
            X_Trimmed (row + 1, 3) := X (I0 (row), 3);
            X_Trimmed (row + 1, 4) := X (I0 (row), 4);
         end loop;

         for row in I1.First_Index .. I1.Last_Index loop
            X_Trimmed (I0_Length + row + 1, 1) := X (I1 (row), 1);
            X_Trimmed (I0_Length + row + 1, 2) := X (I1 (row), 2);
            X_Trimmed (I0_Length + row + 1, 3) := X (I1 (row), 3);
            X_Trimmed (I0_Length + row + 1, 4) := X (I1 (row), 4);
         end loop;

         for row in 1 .. 35 loop
            theDataset.X_Train (row, 1) := X_Trimmed (row, 1);
            theDataset.X_Train (row, 2) := X_Trimmed (row, 2);
            theDataset.X_Train (row, 3) := X_Trimmed (row, 3);
            theDataset.X_Train (row, 4) := X_Trimmed (row, 4);
         end loop;

         for row in 36 .. Train_Length loop
            theDataset.X_Train (row, 1) := X_Trimmed (row + 14, 1);
            theDataset.X_Train (row, 2) := X_Trimmed (row + 14, 2);
            theDataset.X_Train (row, 3) := X_Trimmed (row + 14, 3);
            theDataset.X_Train (row, 4) := X_Trimmed (row + 14, 4);
         end loop;

         for index in 1 .. Train_Length loop
            if index <= 35 then
               theDataset.Y_Train (index, 1) := 0.0;
            else
               theDataset.Y_Train (index, 1) := 1.0;
            end if;
         end loop;

         for row in 1 .. 15 loop
            theDataset.X_Test (row, 1) := X_Trimmed (row + 35, 1);
            theDataset.X_Test (row, 2) := X_Trimmed (row + 35, 2);
            theDataset.X_Test (row, 3) := X_Trimmed (row + 35, 3);
            theDataset.X_Test (row, 4) := X_Trimmed (row + 35, 4);
         end loop;

         for row in 16 .. Test_Length loop
            theDataset.X_Test (row, 1) := X_Trimmed (70 + row, 1);
            theDataset.X_Test (row, 2) := X_Trimmed (70 + row, 2);
            theDataset.X_Test (row, 3) := X_Trimmed (70 + row, 3);
            theDataset.X_Test (row, 4) := X_Trimmed (70 + row, 4);
         end loop;

         for index in 1 .. Test_Length loop
            if index <= 15 then
               theDataset.Y_Test (index, 1) := 0.0;
            else
               theDataset.Y_Test (index, 1) := 1.0;
            end if;
         end loop;
      end;

      Shuffler.Shuffle (theDataset.X_Train, theDataset.Y_Train);
      Shuffler.Shuffle (theDataset.X_Test, theDataset.Y_Test);

      return theDataset;

   end Build_Dataset;

   --  -------------------------------------------------------------------------

   function Means (M : Real_Float_Matrix) return Real_Float_Vector is
      M_Length : constant Float := Float (M'Length);
      Sum1     : Float := 0.0;
      Sum2     : Float := 0.0;
      Sum3     : Float := 0.0;
      Sum4     : Float := 0.0;
      Result   : Real_Float_Vector (M'Range (2));
   begin
      for row in M'Range loop
         Sum1 := Sum1 + M (row, 1);
         Sum2 := Sum2 + M (row, 2);
         Sum3 := Sum3 + M (row, 3);
         Sum4 := Sum4 + M (row, 4);
      end loop;

      for row in M'Range loop
         Result (1) := Sum1 / M_Length;
         Result (2) := Sum2 / M_Length;
         Result (3) := Sum3 / M_Length;
         Result (4) := Sum4 / M_Length;
      end loop;

      return Result;

   end Means;

   --  -------------------------------------------------------------------------

   function Standard_Deviation (M : Real_Float_Matrix)
                                return Real_Float_Vector is
      use maths.Float_Math_Functions;
      M_Length   : constant Float := Float (M'Length);
      Mean_Vals  : constant Real_Float_Vector := Means (M);
      Errors_Sq  : Real_Float_Matrix (M'Range, M'Range (2));
      Sum1       : Float := 0.0;
      Sum2       : Float := 0.0;
      Sum3     : Float := 0.0;
      Sum4     : Float := 0.0;
      SD         : Real_Float_Vector (M'Range (2));
   begin
      for row in M'Range loop
         Errors_Sq (row,1) := (M (row, 1) - Mean_Vals (1)) ** 2;
         Errors_Sq (row,2) := (M (row, 2) - Mean_Vals (2)) ** 2;
         Errors_Sq (row,3) := (M (row, 3) - Mean_Vals (3)) ** 2;
         Errors_Sq (row,4) := (M (row, 4) - Mean_Vals (4)) ** 2;
      end loop;

      for row in M'Range loop
         Sum1 := Sum1 + Errors_Sq (row,1);
         Sum2 := Sum2 + Errors_Sq (row,2);
         Sum3 := Sum3 + Errors_Sq (row,3);
         Sum4 := Sum4 + Errors_Sq (row,4);
      end loop;

      SD (1) := Sqrt (Sum1 / (M_Length - 1.0));
      SD (2) := Sqrt (Sum2 / (M_Length - 1.0));
      SD (3) := Sqrt (Sum3 / (M_Length - 1.0));
      SD (4) := Sqrt (Sum4 / (M_Length - 1.0));

      return SD;

   end Standard_Deviation;

   --  -------------------------------------------------------------------------

end Iris_Support;