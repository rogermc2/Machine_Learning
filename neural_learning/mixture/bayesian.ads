
with IL_Types; use IL_Types;

package Bayesian is

   type Covariance_Type is (Full_Covariance, Tied_Covariance, Diag_Covariance,
                            Spherical_Covariance);
   type Bayesian_Params_Type is (K_Means_Params, Random_Params);
   type Weight_Concentration_Type is
     (Dirichlet_Process, Dirichlet_Distribution);

   type Weight_Concentration_Record is record
--        case Concentration is
--        when Dirichlet_Process =>
         Beta_1    : Float := 0.0;
         Beta_2    : Float := 0.0;  -- not used for Dirichlet_Distribution
--           when Dirichlet_Distribution =>
--              Parameter : Float;
--        end case;
   end record;

   type Bayesian_Gaussian_Parameters is record
      Num_Components                  : Positive := 1;
      Covariance_Kind                 : Covariance_Type := Full_Covariance;
      Tolerance                       : Float := 10.0 ** (-3);
      Reg_Covar                       : Float := 10.0 ** (-6);
      Max_Iter                        : Positive := 100;
      N_Init                          : Positive := 1;
      Init_Params                     : Bayesian_Params_Type := K_Means_Params;
      Weight_Concentration_Prior_Kind : Weight_Concentration_Type :=
                                          Dirichlet_Process;
      Weight_Concentration_Prior      : Float := 0.0;  --  0 means "none"
      Mean_Precision_Prior            : Float := 0.0;  --  0 means "none"
      Mean_Prior                      : Float_List;
      DOG_Prior                       : Float := 0.0;  --  0 means "none"
      Covariance_Prior                : Float_List_2D;
      Random_State                    : Integer := 0;  --  0 means "none"
      Warm_Start                      : Boolean := False;
      Verbose                         : Boolean := False;
      Verbose_Interval                : Positive := 10;
   end record;

   type Bayesian_Gaussian_Attributes is record
      Weights                    : Float_List;     --  n_components
      Means                      : Float_List_2D;  --  n_components x n_features
      Covariances                : Float_List_2D;
      Precisions                 : Float_List_2D;
      Precisions_Cholesky        : Float_List_2D;
      Converged                  : Boolean := False;
      --  N_Iter is the number of the step used by the best fit of inference
      --  to reach the convergence.
      N_Iter                     : Positive;
      --  Lower_Bound is the value on the likelihood of the best fit of
      --  inference.
      Lower_Bound                : Float;
      Weight_Concentration_Prior : Weight_Concentration_Record;
      Weight_Concentration       : Float_List;  --  n_components
      Mean_Precision_Prior       : Float := 1.0;
      Mean_Precision             : Float_List;  --  n_components
      Mean_Prior                 : Float_List;  --  n_features
      Degrees_Of_Freedom         : Float_List;  --  n_components
      Covariance_Prior           : Float_List_2D;
      N_Features_In              : Positive;
      Features_Names_In          : String_List; -- N_Features_In size
   end record;

   type Bayesian_Gaussian_Mixture is record
      Parameters : Bayesian_Gaussian_Parameters;
      Attributes : Bayesian_Gaussian_Attributes;
   end record;

   function Estimate_Log_Weights (Mixture : in out Bayesian_Gaussian_Mixture;
                                  X    : Float_List_2D;
                                  Y    : Integer_List) return Float;
end Bayesian;
