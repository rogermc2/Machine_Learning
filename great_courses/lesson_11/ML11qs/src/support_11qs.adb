
with Python_CLF;

package body Support_11QS is

   function Try_Clusterer
     (Classifier : Python.Module; Num_Clusters : Positive;
      Clusterer  : Python_API.PyObject;
      Train_X    : Real_Float_Matrix; Train_Y : Integer_Matrix) return Float is
      use Python_API;
      Clr    : constant PyObject := Python.Call (Classifier, "clust",
                                                 Num_Clusters);
      Result : Float;
   begin
      Python_CLF.Call (Classifier, "fit", Clr, Train_X);
      declare
         Train_IDs      : constant Integer_Array :=
                            Python_CLF.Call (Classifier, "copy_labels", Clr);
         Cluster_Labels : Integer_Array (1 .. Num_Clusters) := (others => -1);
      begin
         --  Request one label per cluster and make an interim dataset out of
         --  X_train, y_guess .
         for index in Cluster_Labels'Range loop
            Cluster_Labels (index) := Train_Y (Train_IDs (index), 1);
           end loop;
      end;
      return Result;

   end Try_Clusterer;

   --  -------------------------------------------------------------------------

end Support_11QS;
