
with Python_CLF;

package body Support_11QS is

   function Try_Clusterer (Classifier : Python.Module; Num_Clusters : Positive;
                           Clusterer  : Python_API.PyObject;
                           Train_X    : Real_Float_Matrix) return Float is
      use Python_API;
      Clr    : constant PyObject := Python.Call (Classifier, "clust", Num_Clusters);
      Result : Float;
   begin
      Python_CLF.Call (Classifier, "fit", Clr, Train_X);
      declare
         Train_IDS : constant Integer_Array :=
           Python_CLF.Call (Classifier, "copy_labels", Clr);
      begin
         null;
      end;
      return Result;

   end Try_Clusterer;

   --  -------------------------------------------------------------------------

end Support_11QS;
