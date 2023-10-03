
--  with Ada.Assertions; use Ada.Assertions;
--  with Ada.Text_IO; use Ada.Text_IO;

with Maths;

--  with Basic_Printing; use Basic_Printing;
with Classifier_Loader;
with ML_Types;
with NL_Types;
with Type_Utilities;

package body Iris_Support is

   function Means (M : Real_Float_Matrix) return Real_Float_Vector;
   function Standard_Deviation (M : Real_Float_Matrix)
                                return Real_Float_Vector;
   --  -------------------------------------------------------------------------

   function Build_Dataset (Train_Length : Positive := 70;
                           Test_Length  : Positive := 30) return Dataset is
      use NL_Types.Float_Package;
      use NL_Types.Float_List_Package;
      use Type_Utilities;
      --        Routine_Name      : constant String :=
      --                              "Iris_Support.Build_Dataset ";
      Half_Train_Length : constant Positive := Train_Length / 2;
      Half_Test_Length  : constant Positive := Train_Length / 2;
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
        (1 .. Positive (Features.Length), 1 .. 2);
      X_Means           : Real_Float_Vector (X'Range (2));
      X_SDs             : Real_Float_Vector (X'Range (2));
      I0                : ML_Types.Integer_List;
      I1                : ML_Types.Integer_List;
      theDataset        : Dataset (Train_Length, Test_Length, 2);
   begin
      for row in X'Range loop
         Feature_Row := Features (row);
         X (row, 1) := Feature_Row (1);
         X (row, 2) := Feature_Row (2);
      end loop;

      X_Means := Means (X);
      X_SDs := Standard_Deviation (X);
      for row in X'Range loop
         Feature_Row := Features (row);
         X (row, 1) := (X (row, 1) - X_Means (1)) / X_SDs (1);
         X (row, 2) := (X (row, 2) - X_Means (2)) / X_SDs (2);
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
           (1 .. I0_Length + I1_Length, 1 .. 2);
      begin
         for row in I0.First_Index .. I0.Last_Index loop
            X_Trimmed (row + 1, 1) := X (I0 (row), 1);
            X_Trimmed (row + 1, 2) := X (I0 (row), 2);
         end loop;

         for row in I0.First_Index .. I0.Last_Index loop
            X_Trimmed (I0_Length + row + 1, 1) := X (I1 (row), 1);
            X_Trimmed (I0_Length + row + 1, 2) := X (I1 (row), 2);
         end loop;

         for row in 1 .. Half_Train_Length loop
            theDataset.X_Train (row, 1) := X_Trimmed (row, 1);
            theDataset.X_Train (row, 2) := X_Trimmed (row, 2);
         end loop;

         for row in Half_Train_Length + 1 .. Train_Length loop
            theDataset.X_Train (row, 1) := X_Trimmed (row + 14, 1);
            theDataset.X_Train (row, 2) := X_Trimmed (row + 14, 2);
         end loop;

         for index in 1 .. Train_Length loop
            if index <= Half_Train_Length then
               theDataset.Y_Train (index, 1) := 0.0;
            else
               theDataset.Y_Train (index, 1) := 1.0;
            end if;
         end loop;

         for row in 1 .. Half_Test_Length loop
            theDataset.X_Test (row, 1) := X_Trimmed (row + 35, 1);
            theDataset.X_Test (row, 2) := X_Trimmed (row + 35, 2);
         end loop;

         for row in Half_Test_Length + 1 .. Test_Length loop
            theDataset.X_Test (row, 1) := X_Trimmed (row + 70, 1);
            theDataset.X_Test (row, 2) := X_Trimmed (row + 70, 2);
         end loop;

         for index in 1 .. Test_Length loop
            if index <= Half_Test_Length then
               theDataset.Y_Test (index, 1) := 0.0;
            else
               theDataset.Y_Test (index, 1) := 1.0;
            end if;
         end loop;
      end;

      return theDataset;

   end Build_Dataset;

   --  -------------------------------------------------------------------------

   function Means (M : Real_Float_Matrix) return Real_Float_Vector is
      M_Length : constant Float := Float (M'Length);
      Sum1     : Float := 0.0;
      Sum2     : Float := 0.0;
      Result   : Real_Float_Vector (1 .. 2);
   begin
      for row in M'Range loop
         Sum1 := Sum1 + M (row, 1);
         Sum2 := Sum2 + M (row, 2);
      end loop;

      for row in M'Range loop
         Result (1) := Sum1 / M_Length;
         Result (2) := Sum2 / M_Length;
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
      SD         : Real_Float_Vector (1 .. 2);
   begin
      for row in M'Range loop
         Errors_Sq (row,1) := (M (row, 1) - Mean_Vals (1)) ** 2;
         Errors_Sq (row,2) := (M (row, 2) - Mean_Vals (2)) ** 2;
      end loop;

      for row in M'Range loop
         Sum1 := Sum1 + Errors_Sq (row,1);
         Sum2 := Sum2 + Errors_Sq (row,2);
      end loop;

      SD (1) := Sqrt (Sum1 / (M_Length - 1.0));
      SD (2) := Sqrt (Sum2 / (M_Length - 1.0));

      return SD;

   end Standard_Deviation;

   --  -------------------------------------------------------------------------

end Iris_Support;
