
with Ada.Containers.Vectors;

with NL_Types; use NL_Types;
with Base_Neural;
with Estimator;
with Stochastic_Optimizers; use Stochastic_Optimizers;

package Multilayer_Perceptron is

   type Loss_Function is (Log_Loss_Function, Binary_Log_Loss_Function,
                          Squared_Error_Function);

   type Coef_Indptr_Record is record
      Start   : Positive := 1;
      Last    : Positive := 1;
      Fan_In  : Positive := 1;
      Fan_Out : Positive := 1;
   end record;

   package Coef_Indptr_Package is new
     Ada.Containers.Vectors (Positive, Coef_Indptr_Record);
   subtype Coef_Indptr_List is Coef_Indptr_Package.Vector;

   type Intercept_Indptr_Record is record
      Start   : Positive := 1;
      Last    : Positive := 1;
   end record;

   package Intercept_Indptr_Package is new
     Ada.Containers.Vectors (Positive, Intercept_Indptr_Record);
   subtype Intercept_Indptr_List is Intercept_Indptr_Package.Vector;

   type MLP_Classifier_Attributes is record
      Classes              : Integer_List;
      Loss_Function_Name   : Loss_Function;
      Loss                 : Float;
      Best_Loss            : Float;
      Loss_Curve           : Float_List;
      No_Improvement_Count : Natural := 0;
      T                    : Natural;
      --  Neuron_Coef_Layers layers x rows x values
      Neuron_Coef_Layers   : Float_List_3D;
      Coef_Indptr          : Coef_Indptr_List;
      --  Intercepts: layers x values
      Intercepts           : Float_List_2D;
      Intercept_Indptr     : Intercept_Indptr_List;
      N_Features           : Positive;
      Feature_Names_In     : String_List;
      N_Iter               : Natural;
      N_Layers             : Positive;
      N_Outputs            : Positive := 1;
      Out_Activation       : Base_Neural.Activation_Type :=
                               Base_Neural.Logistic_Activation;
      Optimizer            : Optimizer_Record;
   end record;

   type MLP_Classifier_Parameters is record
      Hidden_Layer_Sizes    : Integer_List;
      Activation            : Base_Neural.Activation_Type :=
                                Base_Neural.Relu_Activation;
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
      Beta_1                : Float := 0.9;
      Beta_2                : Float := 0.999;
      Epsilon               : Float := 10.0 ** (-8);
      N_Iter_No_Change      : Natural := 10;
      Max_Fun               : Natural := 15000;
   end record;

   type MLP_Classifier is record
      Estimator_Kind : Estimator.Estimator_Type :=
                         Estimator.Classifier_Estimator;
      Attributes     : MLP_Classifier_Attributes;
      Parameters     : MLP_Classifier_Parameters;
   end record;

   function C_Init (Hidden_Layer_Sizes  : Integer_List :=
                      Integer_Package.Empty_Vector;
                    Activation          : Base_Neural.Activation_Type :=
                      Base_Neural.Relu_Activation;
                    Solver              : Solver_Type := Adam_Solver;
                    Alpha               : Float := 0.0001;
                    Batch_Size          : Positive := 200;
                    Learning_Rate       : Float := 0.001;
                    Learning_Rate_Init  : Float := 0.001;
                    Power_T             : Float := 0.5;
                    Max_Iter            : Natural := 200;
                    Shuffle             : Boolean := True;
                    Random_State        : Natural := 0;
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
                    N_Iter_No_Change    : Natural := 10;
                    Max_Fun             : Natural := 15000)
                    return MLP_Classifier;
   procedure Fit (Self : in out MLP_Classifier;
                  X    : Float_List_2D;
                  Y    : Integer_List;
                  Incremental : Boolean := False);

end Multilayer_Perceptron;
