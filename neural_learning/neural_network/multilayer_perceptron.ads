
--  with Ada.Containers.Vectors;

with Base_Neural;
with Estimator;
with Label;
with NL_Arrays_And_Matrices; use NL_Arrays_And_Matrices;
with NL_Types; use NL_Types;
with Num_Diff;
with Stochastic_Optimizers; use Stochastic_Optimizers;

package Multilayer_Perceptron is

   type Loss_Function_Type is (Log_Loss_Function, Binary_Log_Loss_Function,
                               Squared_Error_Function);

   type Max_Function_Access is access function
     (X : Parameters_List; Num : Integer := 15000) return Integer;

   type MLP_Classifier_Attributes is record
      Classes              : Integer_List;
      Loss_Function_Name   : Loss_Function_Type := Log_Loss_Function;
      Loss                 : Float := 0.0;
      Best_Loss            : Float := 0.0;
      Loss_Curve           : Float_List;
      No_Improvement_Count : Natural := 0;
      T                    : Natural := 0;
      Params               : Parameters_List;  --  Layers
      --  Params list of n_layers of Parameter records each record comprising:
      --  a coefs_list of rows x coeff values representing the weight matrix
      --  corresponding to layer i.
      --  an intercepts_list of intercept values representing the bias vector
      --  corresponding to layer i + 1.
      --          Coef_Indptr          : Coef_Indptr_List;
      --          Intercept_Indptr     : Intercept_Indptr_List;
      N_Features           : Positive := 1;
      Feature_Names_In     : String_List;
      N_Iter               : Natural := 0;
      N_Layers             : Positive := 1;
      N_Outputs            : Positive := 1;
      Out_Activation       : Base_Neural.Activation_Type :=
                               Base_Neural.Softmax_Activation;
      Optimizer            : Optimizer_Record;
      Binarizer            : Label.Label_Binarizer;
   end record;

   type MLP_Classifier_Parameters is
      record
         Hidden_Layer_Sizes    : Integer_List;
         Activation            : Base_Neural.Activation_Type :=
                                   Base_Neural.Rect_LU_Activation;
         Solver                : Solver_Type := Adam_Solver;
         Alpha                 : Float := 0.0001;
         Batch_Size            : Natural := 0;  --  0 -> "auto"
         Learning_Rate_Kind    : Learning_Rate_Type := Constant_Rate;
         Learning_Rate_Init    : Float := 0.001;
         Learning_Rate         : Float := 0.001;
         Power_T               : Float := 0.5;
         Max_Iter              : Natural := 200;
         Shuffle               : Boolean := True;
         Random_State          : Natural;
         Tol                   : Float := 10.0 ** (-4);
         Verbose               : Boolean := False;
         Warm_Start            : Boolean := False;
         Momentum              : Float := 0.9;
         Nesterovs_Momentum    : Boolean := True;
         Early_Stopping        : Boolean := False;
         Validation_Scores     : Float_List;
         Validation_Fraction   : Float := 0.1;
         Best_Validation_Score : Float := Float'Safe_First;
         Best_Params           : Parameters_List;
         Beta_1                : Float := 0.9;
         Beta_2                : Float := 0.999;
         Epsilon               : Float := 10.0 ** (-8);
         N_Iter_No_Change      : Natural := 10;
         Max_Fun               : Max_Function_Access := null;
         RF_Fun                : Num_Diff.Deriv_Float_Fun_Access := null;
      end record;

   type MLP_Classifier is
      record
         Estimator_Kind : Estimator.Estimator_Type :=
                            Estimator.Classifier_Estimator;
         Attributes     : MLP_Classifier_Attributes;
         Parameters     : MLP_Classifier_Parameters;
      end record;

   type Loss_Grad_Args (Num_Rows, Num_X_Cols, Num_Y_Cols : Positive) is record
      Self        : Multilayer_Perceptron.MLP_Classifier;
      Params      : Parameters_List;
      X           : Real_Float_Matrix (1 .. Num_Rows, 1 .. Num_X_Cols);
      Y           : Binary_Matrix (1 .. Num_Rows, 1 .. Num_Y_Cols);
      Activations : Real_Matrix_List;
      Gradients   : Parameters_List;
   end record;

   type Loss_Grad_Result is record
      Loss       : Float;
      Parameters : Parameters_List;
   end record;
   function "-" (L, R : Loss_Grad_Result) return Loss_Grad_Result;
   function "/" (L : Loss_Grad_Result; R : Float) return Loss_Grad_Result;

   type Loss_Grad_Access is access function (Args : Loss_Grad_Args)
                                              return Loss_Grad_Result;

   function C_Init (Hidden_Layer_Sizes    : Integer_List :=
                      Integer_Package.Empty_Vector;
                    Activation            : Base_Neural.Activation_Type :=
                      Base_Neural.Rect_LU_Activation;
                    Solver                : Solver_Type := Adam_Solver;
                    Alpha                 : Float := 0.0001;
                    Batch_Size            : Positive := 200;
                    Learning_Rate         : Float := 0.001;
                    Learning_Rate_Init    : Float := 0.001;
                    Power_T               : Float := 0.5;
                    Max_Iter              : Natural := 200;
                    Loss_Function_Name    : Loss_Function_Type :=
                      Log_Loss_Function;
                    Shuffle               : Boolean := True;
                    Random_State          : Natural := 0;
                    Tol                   : Float := 10.0 ** (-4);
                    Verbose               : Boolean := False;
                    Warm_Start            : Boolean := False;
                    Momentum              : Float := 0.9;
                    Nesterovs_Momentum    : Boolean := True;
                    Early_Stopping        : Boolean := False;
                    Validation_Fraction   : Float := 0.1;
                    Beta_1                : Float := 0.9;
                    Beta_2                : Float := 0.999;
                    Epsilon               : Float := 10.0 ** (-8);
                    N_Iter_No_Change      : Natural := 10;
                    Max_Fun               : Max_Function_Access := null;
                    RF_Fun                : Num_Diff.Deriv_Float_Fun_Access
                    := null)
                     return MLP_Classifier;
   procedure Fit (Self  : in out MLP_Classifier;
                  X     : Real_Float_Matrix;
                  Y     : Integer_Matrix; Incremental : Boolean := False);
   procedure Init_Optimizer (Self : in out MLP_Classifier);
   function Loss_Grad_LBFGS (Args : Loss_Grad_Args) return Loss_Grad_Result;
   procedure Partial_Fit (Self : in out MLP_Classifier; X : Real_Float_Matrix;
                           Y    : Binary_Matrix; Classes : Integer_List);
   procedure Partial_Fit (Self : in out MLP_Classifier; X : Real_Float_Matrix;
                          Y    : Integer_Matrix);
   procedure Partial_Fit
     (Self : in out MLP_Classifier; X : Real_Float_Matrix;
      Y       : Integer_Matrix; Classes : Integer_List);
   function Predict (Self : MLP_Classifier; X : Real_Float_Matrix)
                      return Integer_Matrix;
   function Predict_ProbA (Self : MLP_Classifier; X : Real_Float_Matrix)
                     return Real_Float_Matrix;
   function Validate_Input (Self        : in out MLP_Classifier;
                            Y           : Binary_Matrix;
                            Incremental : Boolean) return Binary_Matrix;
   function Validate_Input (Self        : in out MLP_Classifier;
                            Y           : Integer_Matrix;
                            Incremental : Boolean) return Binary_Matrix;

end Multilayer_Perceptron;
