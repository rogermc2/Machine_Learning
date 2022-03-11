--  Based on scikit-learn/sklearn/neural_network/_multilayer_perceptron.py
with Ada.Assertions; use Ada.Assertions;
with Ada.Containers;

with Encode_Utils;

package body Multilayer_Perceptron is

   First_Pass : Boolean := True;

   procedure Validate_Hyperparameters (Self : MLP_Classifier);
   procedure Initialize (Self        : in out MLP_Classifier;
                         Y           : IL_Types.Integer_List;
                         Layer_Units : IL_Types.String_Vector);
   procedure Validate_Input (Self               : in out MLP_Classifier;
                             --                               X                  : IL_Types.Float_List_2D;
                             Y                  : IL_Types.Integer_List);
   --                               Incremental, Reset : Boolean);

   --  -------------------------------------------------------------------------

   function C_Init (Hidden_Layer_Sizes  : IL_Types.Integer_List :=
                      IL_Types.Integer_Package.Empty_Vector;
                    Activation          : Activation_Type := Relu_Activation;
                    Solver              : Solver_Type := Adam_Solver;
                    Alpha               : Float := 0.0001;
                    Batch_Size          : Positive := 200;
                    Learning_Rate       : Learning_Rate_Type := Constant_Rate;
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
                    Max_Fun             : Natural := 15000) return MLP_Classifier is
      Classifier : MLP_Classifier;
   begin
      Classifier.Parameters.Hidden_Layer_Sizes  := Hidden_Layer_Sizes;
      Classifier.Parameters.Activation          := Activation;
      Classifier.Parameters.Solver              := Solver;
      Classifier.Parameters.Alpha               := Alpha;
      Classifier.Parameters.Batch_Size          := Batch_Size;
      Classifier.Parameters.Learning_Rate       := Learning_Rate;
      Classifier.Parameters.Learning_Rate_Init  := Learning_Rate_Init;
      Classifier.Parameters.Power_T             := Power_T;
      Classifier.Parameters.Max_Iter            := Max_Iter;
      Classifier.Parameters.Shuffle             := Shuffle;
      Classifier.Parameters.Random_State        := Random_State;
      Classifier.Parameters.Tol                 := Tol;
      Classifier.Parameters.Verbose             := Verbose;
      Classifier.Parameters.Warm_Start          := Warm_Start;
      Classifier.Parameters.Momentum            := Momentum;
      Classifier.Parameters.Nesterovs_Momentum  := Nesterovs_Momentum;
      Classifier.Parameters.Early_Stopping      := Early_Stopping;
      Classifier.Parameters.Validation_Fraction := Validation_Fraction;
      Classifier.Parameters.Beta_1              := Beta_1;
      Classifier.Parameters.Beta_2              := Beta_2;
      Classifier.Parameters.Epsilon             := Epsilon;
      Classifier.Parameters.N_Iter_No_Change    := N_Iter_No_Change;
      Classifier.Parameters.Max_Fun             := Max_Fun;
      First_Pass                                := True;
      return Classifier;
   end C_Init;

   --  -------------------------------------------------------------------------
   --  L377  BaseMultilayerPerceptron._Fit
   procedure Fit (Self        : in out MLP_Classifier;
                  X           : IL_Types.Float_List_2D;
                  Y           : IL_Types.Integer_List;
                  Incremental : Boolean := False) is
      use Ada.Containers;
      use IL_Types;
      Routine_Name              : constant String := "Multilayer_Perceptron.Fit ";
      Hidden_Layer_Sizes_Length : constant Count_Type :=
                                    Self.Parameters.Hidden_Layer_Sizes.Length;
      Hidden_Layer_Sizes        : Integer_List;
      Layer_Units               : String_Vector;
   begin
      Assert (Hidden_Layer_Sizes_Length > 0,
              Routine_Name & "Hidden_Layer_Sizes vector is empty");
      Hidden_Layer_Sizes.Set_Length (Hidden_Layer_Sizes_Length);
      Validate_Hyperparameters (Self);
      First_Pass :=
        Self.Attributes.Coefs.Is_Empty or else
        (not Self.Parameters.Warm_Start and then not Incremental);

      Layer_Units.Set_Length (Count_Type (Self.Attributes.N_Features));
      Validate_Input (Self, Y);

      if First_Pass then
         Initialize (Self, Y, Layer_Units);
      end if;

   end Fit;

   --  -------------------------------------------------------------------------
   --  L320  BaseMultilayerPerceptron._Initialize
   procedure Initialize (Self        : in out MLP_Classifier;
                         Y           : IL_Types.Integer_List;
                         Layer_Units : IL_Types.String_Vector) is
      use IL_Types;
      use Weights;
      Routine_Name : constant String := "Multilayer_Perceptron.Fit ";
   begin
      Self.Attributes.N_Iter := 0;
      Self.Attributes.T := 0;
      Self.Attributes.N_Layers := Natural (Layer_Units.Length);
      Self.Attributes.Out_Activation := Logistic_Activation;
      Self.Attributes.Coefs.Clear;

   end Initialize;

   --  -------------------------------------------------------------------------
   -- L455
   procedure Validate_Hyperparameters (Self : MLP_Classifier) is
      --                               Incremental, Reset : Boolean) is
      --        Routine_Name : constant String := "Multilayer_Perceptron.Validate_Hyperparameters ";
   begin
         null;

   end Validate_Hyperparameters;

   --  -------------------------------------------------------------------------

   procedure Validate_Input (Self               : in out MLP_Classifier;
                             --                               X                  : IL_Types.Float_List_2D;
                             Y                  : IL_Types.Integer_List) is
      --                               Incremental, Reset : Boolean) is
      --        Routine_Name : constant String := "Multilayer_Perceptron.Validate_Input ";
   begin
      if Self.Attributes.Classes.Is_Empty and then
        Self.Parameters.Warm_Start then
         Self.Attributes.Classes := Encode_Utils.Unique (Y);
      else
         null;
      end if;

   end Validate_Input;

   --  -------------------------------------------------------------------------

end Multilayer_Perceptron;
