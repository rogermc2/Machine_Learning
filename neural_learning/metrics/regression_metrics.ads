
with Classifier_Types;
with NL_Types;
with Weights;

package Regression_Metrics is

   type Multioutput_Type is (MO_Uniform_Average, MO_Raw_Values);

   function Mean_Squared_Error
     (Y_True, Y_Prediction : NL_Types.Value_Data_Lists_2D;
      Sample_Weight        : Weights.Weight_List :=
        Classifier_Types.Float_Package.Empty_Vector;
      Multioutput          : Multioutput_Type := MO_Uniform_Average;
      Squared              : Boolean := True)
      return Classifier_Types.Float_List;

end Regression_Metrics;
