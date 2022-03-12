
with IL_Types;
with Stochastic_Optimizers; use Stochastic_Optimizers;

package Multilayer_Perceptron is

   type Activation_Type is (Identity_Activation, Logistic_Activation,
                            Tanh_Activation, Relu_Activation);

   type MLP_Classifier_Attributes is record
      Classes              : IL_Types.Integer_List;
      Loss                 : Float;
      Best_Loss            : Float;
      Loss_Curve           : IL_Types.Float_List;
      No_Improvement_Count : Natural := 0;
      T                    : Natural;
      Coefs                : IL_Types.Float_List_3D;
      Intercepts           : IL_Types.Float_List_2D;
      N_Features           : Positive;
      Feature_Names_In     : IL_Types.String_List;
      N_Iter                : Natural;
      N_Layers             : Positive;
      N_Outputs            : Positive;
      Out_Activation       : Activation_Type := Logistic_Activation;
      Has_Optimizer        : Optimizer_Type := No_Optimizer;
      Optimizer            : Optimizer_Record;
   end record;

   type MLP_Classifier_Parameters is record
      Hidden_Layer_Sizes    : IL_Types.Integer_List;
      Activation            : Activation_Type := Relu_Activation;
      Is_Classifier         : Boolean := True;
      Solver                : Solver_Type := Adam_Solver;
      Alpha                 : Float := 0.0001;
      Batch_Size            : Positive;
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
      Validation_Scores     : IL_Types.Float_List;
      Validation_Fraction   : Float := 0.1;
      Best_Validation_Score : Float := Float'Safe_First;
      Beta_1                : Float := 0.9;
      Beta_2                : Float := 0.999;
      Epsilon               : Float := 10.0 ** (-8);
      N_Iter_No_Change      : Natural := 10;
      Max_Fun               : Natural := 15000;
   end record;

   type MLP_Classifier is record
      Attributes : MLP_Classifier_Attributes;
      Parameters : MLP_Classifier_Parameters;
   end record;

   function C_Init (Hidden_Layer_Sizes  : IL_Types.Integer_List :=
                      IL_Types.Integer_Package.Empty_Vector;
                    Activation          : Activation_Type := Relu_Activation;
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
                  X    : IL_Types.Float_List_2D;
                  Y    : IL_Types.Integer_List;
                  Incremental : Boolean := False);

end Multilayer_Perceptron;
