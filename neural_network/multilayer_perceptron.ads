
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with ML_Types;
with Weights;

package Multilayer_Perceptron is

   type Activation_Type is (Identity_Activation, Logistic_Activation,
                            Tanh_Activation, Relu_Activation);
   type Solver_Type is (Lbfgs_Solver, Sgd_Solver, Adam_Solver);
   type Learning_Rate_Type is (Constant_Rate, Invscaling_Rate, Adaptive_Rate);

   type MLP_Classifier_Attributes is record
      Classes          : ML_Types.Value_Data_Lists_2D;
      Loss             : Float;
      Best_Loss        : Float;
      Loss_Curve       : ML_Types.Float_List;
      T                : Integer;
      Coefs            : Weights.Weight_Lists_2D;
      Intercepts       : ML_Types.Float_List_2D;
      N_Features       : Integer;
      Feature_Names_In : ML_Types.Feature_Names_List;
      N_Iter           : Integer;
      N_Layers         : Integer;
      N_Outputs        : Integer;
      Out_Activation   : Unbounded_String := To_Unbounded_String ("");
   end record;

   type MLP_Classifier_Parameters is record
      Hidden_Layer_Sizes  : ML_Types.Integer_List;
      Activation          : Activation_Type := Relu_Activation;
      Solver              : Solver_Type := Adam_Solver;
      Alpha               : Float := 0.0001;
      Batch_Size          : Positive;
      Learning_Rate       : Learning_Rate_Type := Constant_Rate;
      Learning_Rate_Init  : Float := 0.001;
      Power_T             : Float := 0.5;
      Max_Iter            : Integer := 200;
      Shuffle             : Boolean := True;
      Random_State        : Integer;
      Tol                 : Float := 10.0 ** (-4);
      Verbose             : Boolean := False;
      Warm_Start          : Boolean := False;
      Momentum            : Float := 0.9;
      Nesterovs_Momentum  : Boolean := True;
      Early_Stopping      : Boolean := False;
      Validation_Fraction : Float := 0.1;
      Beta_1              : Float := 0.9;
      Beta_2              : Float := 0.999;
      Epsilon             : Float := 10.0 ** (-8);
      N_Iter_No_Change    : Integer := 10;
      Max_Fun             : Integer := 15000;
   end record;

   type MLP_Classifier is record
      Attributes : MLP_Classifier_Attributes;
      Parameters : MLP_Classifier_Parameters;
   end record;

   function C_Init ( Hidden_Layer_Sizes : ML_Types.Integer_List;
      Activation          : Activation_Type := Relu_Activation;
      Solver              : Solver_Type := Adam_Solver;
      Alpha               : Float := 0.0001;
      Batch_Size          : Positive;
      Learning_Rate       : Learning_Rate_Type := Constant_Rate;
      Learning_Rate_Init  : Float := 0.001;
      Power_T             : Float := 0.5;
      Max_Iter            : Integer := 200;
      Shuffle             : Boolean := True;
      Random_State        : Integer;
      Tol                 : Float := 10.0 ** (-4);
      Verbose             : Boolean := False;
      Warm_Start          : Boolean := False;
      Momentum            : Float := 0.9;
      Nesterovs_Momentum  : Boolean := True;
      Early_Stopping      : Boolean := False;
      Validation_Fraction : Float := 0.1;
      Beta_1              : Float := 0.9;
      Beta_2              : Float := 0.999;
      Epsilon             : Float := 10.0 ** (-8);
      N_Iter_No_Change    : Integer := 10;
      Max_Fun             : Integer := 15000) return MLP_Classifier;

end Multilayer_Perceptron;
