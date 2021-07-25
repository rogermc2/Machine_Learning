
package Estimator is

   type Estimator_Type is (Classifier_Estimator, Regressor_Estimator,
                           Outlier_Detector_Estimator, Unspecified_Estimator);

   type Boolean_Matrix is array
     (Integer range <>, Integer range <>) of Boolean;

   type Estimator_Data (Rows, Cols : Integer) is record
      Kind                : Estimator_Type := Unspecified_Estimator;
      Non_Deterministic   : Boolean := False;
      Requires_Positive_X : Boolean := False;
      Requires_Positive_Y : Boolean := False;
      X_Types             : Boolean_Matrix (1 .. Rows, 1 .. Cols) :=
                              (others => (others => False));
      Poor_Score          : Boolean := False;
      No_Validation       : Boolean := False;
      Multioutput         : Boolean := False;
      Allow_Nan           : Boolean := False;
      Stateless           : Boolean := False;
      Multilabel          : Boolean := False;
      Skip_Test           : Boolean := False;
      Xfail_Checks        : Boolean := False;
      Multioutput_Only    : Boolean := False;
      Binary_Only         : Boolean := False;
      Requires_Fit        : Boolean := True;
      Requires_Y          : Boolean := False;
   end record;

end Estimator;
