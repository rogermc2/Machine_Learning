
--  with Ada.Text_IO; use Ada.Text_IO;

with Basic_Printing; use  Basic_Printing;
with ML_Types;
with Python_API;
with Python_CLF;

package body Support_11QS is

   function Get_ID (Train_IDs : Integer_Array; ID : Positive)
                    return Integer_Array is
      ID_List : ML_Types.Integer_List;
   begin
      --  Train_IDs values start at 0.
      for index in Train_IDs'Range loop
         if Train_IDs (index) = ID then
            ID_List.Append (index + 1);
         end if;
      end loop;

      return To_Integer_Array (ID_List);

   end Get_ID;

   --  -------------------------------------------------------------------------

   function Get_IDs (Train_Vals : Integer_Matrix; IDs : Integer_Array)
                     return ML_Types.Integer_List is
      Result : ML_Types.Integer_List;
   begin
      if IDs'Length > 0 then
         for index in IDs'Range loop
            Result.Append (Train_Vals (IDs (index) - 1, 1));
         end loop;
      end if;

      return Result;

   end Get_IDs;

   --  -------------------------------------------------------------------------

   function Try_Clusterer
     (Classifier      : Python.Module; Num_Clusters : Positive;
      Train_X, Test_X : Real_Float_Matrix;
      Train_Y, Test_Y : Integer_Matrix)
      return Float is
      use Python_API;
      Routine_Name   : constant String := "Support_11QS.Try_Clusterer ";
      K_Means        : constant PyObject := Python.Call (Classifier, "kmeans_fit",
                                                         Num_Clusters, Train_X);
      Train_IDs      : constant Integer_Array :=
                         Python_CLF.Call (Classifier, "labels", K_Means);
      Cluster_Labels : Integer_Array (1 .. Num_Clusters) := (others => -1);
      Y_Guess        : Integer_Array (Train_X'Range);
      Y_Pred         : Integer_Array (Test_X'Range);
      Result         : Natural := 0;
   begin
      --  Request one label per cluster and make an interim dataset out of
      --  X_train, y_guess.
      --  Train_IDs and Cluster_Labels values start at 0.
      for index in Cluster_Labels'Range loop
         declare
            D : constant Integer_Array := Get_ID (Train_IDs, index);
            E : constant ML_Types.Integer_List := Get_IDs (Train_Y, D);
         begin
            for index in E.First_Index .. E.Last_Index loop
               Cluster_Labels (E (index) + 1) := Train_Y (index, 1);
            end loop;
         end;
      end loop;

      for index in Y_Guess'Range loop
         Y_Guess (index) := Cluster_Labels (Train_IDs (index) + 1);
      end loop;

      Y_Pred := Python.Call (Classifier, "y_pred", Train_X, Test_X, Y_Guess);
      Print_Integer_Array (Routine_Name & "Y_Pred", Y_Pred, 1, 10);
      Print_Integer_Matrix (Routine_Name & "Test_Y", Test_Y, 1, 10);

      for index in Y_Pred'Range loop
         if Y_Pred (index) = Test_Y (index, 1) then
            Result := Result + 1;
         end if;
      end loop;

      return Float (Result) / Float (Test_Y'Length);

   end Try_Clusterer;

   --  -------------------------------------------------------------------------

end Support_11QS;
