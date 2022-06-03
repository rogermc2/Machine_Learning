
with Ada.Containers.Vectors;

with Label;
with NL_Arrays_And_Matrices; use NL_Arrays_And_Matrices;
with NL_Types;
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
        Classes              : NL_Types.Integer_List;
        Loss_Function_Name   : Loss_Function := Log_Loss_Function;
        Loss                 : Float := 0.0;
        Best_Loss            : Float := 0.0;
        Loss_Curve           : NL_Types.Float_List;
        No_Improvement_Count : Natural := 0;
        T                    : Natural := 0;
        Params               : Parameters_List;  --  Layers
        --  Params list of n_layers of Parameter records each record comprising:
        --  a coefs_list of rows x coeff values representing the weight matrix
        --  corresponding to layer i.
        --  an intercepts_list of intercept values representing the bias vector
        --  corresponding to layer i + 1.
        Coef_Indptr          : Coef_Indptr_List;
        Intercept_Indptr     : Intercept_Indptr_List;
        N_Features           : Positive := 1;
        Feature_Names_In     : NL_Types.String_List;
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
           Hidden_Layer_Sizes    : NL_Types.Integer_List;
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
           Validation_Scores     : NL_Types.Float_List;
           Validation_Fraction   : Float := 0.1;
           Best_Validation_Score : Float := Float'Safe_First;
           Best_Params           : Parameters_List;
           Beta_1                : Float := 0.9;
           Beta_2                : Float := 0.999;
           Epsilon               : Float := 10.0 ** (-8);
           N_Iter_No_Change      : Natural := 10;
           Max_Fun               : Natural := 15000;
       end record;

    type MLP_Classifier is
       record
           Estimator_Kind : Estimator.Estimator_Type :=
                              Estimator.Classifier_Estimator;
           Attributes     : MLP_Classifier_Attributes;
           Parameters     : MLP_Classifier_Parameters;
       end record;

    function C_Init (Hidden_Layer_Sizes  : NL_Types.Integer_List :=
                       NL_Types.Integer_Package.Empty_Vector;
                     Activation          : Base_Neural.Activation_Type :=
                       Base_Neural.Rect_LU_Activation;
                     Solver              : Solver_Type := Adam_Solver;
                     Alpha               : Float := 0.0001;
                     Batch_Size          : Positive := 200;
                     Learning_Rate       : Float := 0.001;
                     Learning_Rate_Init  : Float := 0.001;
                     Power_T             : Float := 0.5;
                     Max_Iter            : Natural := 200;
                     Loss_Function_Name  : Loss_Function := Log_Loss_Function;
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
                   X    : Real_Float_Matrix;
                   Y    : Integer_Matrix;
                   Incremental : Boolean := False);
    procedure Init_Optimizer (Self : in out MLP_Classifier);
    function Loss_Grad_LBFGS (Self : in out MLP_Classifier;
                              X    : Real_Float_Matrix;
                              Y    : Boolean_Matrix;
                              Activations : in out Real_Matrix_List;
                              Gradients   : out Parameters_List) return Float;
    procedure Partial_Fit (Self : in out MLP_Classifier; X : Real_Float_Matrix;
                           Y    : Integer_Matrix);
    procedure Partial_Fit
      (Self : in out MLP_Classifier; X : Real_Float_Matrix;
       Y    : Integer_Matrix; Classes : NL_Types.Integer_List);
    function Predict (Self : MLP_Classifier; X : Real_Float_Matrix)
                      return Real_Float_Matrix;

end Multilayer_Perceptron;
