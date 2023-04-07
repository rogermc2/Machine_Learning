
with Ada.Text_IO; use Ada.Text_IO;

with Python_CLF;

package body Support_11QS is

   function Try_Clusterer
     (Classifier : Python.Module; Num_Clusters : Positive;
      Train_X    : Real_Float_Matrix; Train_Y : Integer_Matrix) return Float is
      use Python_API;
      Routine_Name : constant String := "Support_11QS.Try_Clusterer ";
      K_Means      : constant PyObject := Python.Call (Classifier, "kmeans_fit",
                                                       Num_Clusters, Train_X);
      Train_IDs    : constant Integer_Array :=
        Python_CLF.Call (Classifier, "labels", K_Means);
      Cluster_Labels : Integer_Array (1 .. Num_Clusters) := (others => -1);
      Y_Guess        : Integer_Array (Train_IDs'Range);
      Result : Float;
   begin
      --  Request one label per cluster and make an interim dataset out of
      --  X_train, y_guess.
      --  Train_IDs values start at 0.
      for index in Cluster_Labels'Range loop
         Cluster_Labels (index) := Train_Y (Train_IDs (index) + 1, 1);
      end loop;

      for index in Y_Guess'Range loop
         Y_Guess (index) := Cluster_Labels (Train_IDs (index) + 1);
      end loop;

      return Result;

   end Try_Clusterer;

   --  -------------------------------------------------------------------------

end Support_11QS;
