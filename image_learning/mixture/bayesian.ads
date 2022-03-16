
with IL_Types; use IL_Types;

package Bayesian is

   type Covariance_Type is (Full_Covariance, Tied_Covariance, Diag_Covariance,
                            Spherical_Covariance);
   type Bayesian_Params_Type is (K_Means_Params, Random_Params);
   type Weight_Concentration_Type is (Dirichlet_Process);

   type Bayesian_Gaussian_Mixture is record
      Num_Components             : Positive := 1;
      Covariance_Kind            : Covariance_Type := Full_Covariance;
      Tolerance                  : Float := 10.0 ** (-3);
      Reg_Covar                  : Float := 10.0 ** (-6);
      Max_Iter                   : Positive := 100;
      N_Init                     : Positive := 1;
      Init_Params                : Bayesian_Params_Type := K_Means_Params;
      Weight_Concentration_Prior_Kind : Weight_Concentration_Type :=
                                          Dirichlet_Process;
      Weight_Concentration_Prior : Float := 0.0;  --  0 means "none"
      Mean_Precision_Prior       : Float := 0.0;  --  0 means "none"
   end record;

   function Estimate_Log_Weights (X    : Float_List_2D;
                   Y    : Integer_List) return Float;
end Bayesian;
