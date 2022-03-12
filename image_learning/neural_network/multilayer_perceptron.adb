--  Based on scikit-learn/sklearn/neural_network/_multilayer_perceptron.py
with Ada.Assertions; use Ada.Assertions;
with Ada.Containers;

with Maths;
with Utilities;

with Encode_Utils;

package body Multilayer_Perceptron is

   First_Pass : Boolean := True;

   procedure Fit_Stochastic (Self            : in out MLP_Classifier;
                             X               : IL_Types.Float_List_2D;
                             Y               : IL_Types.Integer_List;
                             Activations     : IL_Types.Float_List_2D;
                             Deltas          : IL_Types.Float_List;
                             Coef_Grads      : IL_Types.Float_List_3D;
                             Intercept_Grads : IL_Types.Float_List_2D;
                             Layer_Units     : IL_Types.Integer_List;
                             Incremental     : Boolean := False);
   procedure Initialize (Self        : in out MLP_Classifier;
                         Layer_Units : IL_Types.Integer_List);
   procedure Init_Coeff (Self            : in out MLP_Classifier;
                         Fan_In, Fan_Out : Positive;
                         Coef_Init       : out IL_Types.Float_List_2D;
                         Intercept_Init  : out IL_Types.Float_List);
   procedure Init_Coeff_Grads (Layer_Units     : IL_Types.Integer_List;
                               Coef_Grads      : out IL_Types.Float_List_3D;
                               Intercept_Grads : out IL_Types.Float_List_2D);
   procedure Validate_Hyperparameters (Self : MLP_Classifier);
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
      Routine_Name              : constant String :=
                                    "Multilayer_Perceptron.Fit ";
--        Num_Samples               : constant Positive := Positive (X.Length);
      Num_Features              : constant Positive :=
                                    Positive (X.Element (1).Length);
      Hidden_Layer_Sizes_Length : constant Count_Type :=
                                    Self.Parameters.Hidden_Layer_Sizes.Length;
      Activations               : Float_List_2D := X;
      Hidden_Layer_Sizes        : Integer_List;
      Layer_Units               : Integer_List;
      Deltas                    : Float_List;
      Coef_Grads                : Float_List_3D;
      Intercept_Grads           : Float_List_2D;

   begin
      Assert (Hidden_Layer_Sizes_Length > 0,
              Routine_Name & "Hidden_Layer_Sizes vector is empty");
      Hidden_Layer_Sizes.Set_Length (Hidden_Layer_Sizes_Length);
      Validate_Hyperparameters (Self);
      First_Pass :=
        Self.Attributes.Coefs.Is_Empty or else
        (not Self.Parameters.Warm_Start and then not Incremental);

      Layer_Units.Append (Num_Features);
      for index in Hidden_Layer_Sizes.First_Index ..
        Hidden_Layer_Sizes.Last_Index loop
         Layer_Units.Append (Hidden_Layer_Sizes.Element (index));
      end loop;

      Validate_Input (Self, Y);

      if First_Pass then
         Initialize (Self, Layer_Units);
      end if;

      Activations.Set_Length (X.Length + Layer_Units.Length);
      Deltas.Set_Length (Activations.Length);

      Init_Coeff_Grads (Layer_Units, Coef_Grads, Intercept_Grads);

      if Self.Parameters.Solver = Sgd_Solver or else
          Self.Parameters.Solver = Adam_Solver then
          null;
      else
            null;
      end if;

   end Fit;

   --  -------------------------------------------------------------------------

   --  L563
   procedure Fit_Stochastic (Self            : in out MLP_Classifier;
                             X               : IL_Types.Float_List_2D;
                             Y               : IL_Types.Integer_List;
                             Activations     : IL_Types.Float_List_2D;
                             Deltas          : IL_Types.Float_List;
                             Coef_Grads      : IL_Types.Float_List_3D;
                             Intercept_Grads : IL_Types.Float_List_2D;
                             Layer_Units     : IL_Types.Integer_List;
                             Incremental     : Boolean := False) is

        use IL_Types;
        use Stochastic_Optimizers;
        use List_Of_Float_Lists_Package;
        Params    : Float_List_3D :=
                      Self.Attributes.Coefs & Self.Attributes.Intercepts;
   begin
      if not Incremental or else
          Self.Attributes.Has_Optimizer = No_Optimizer then
         case Self.Parameters.Solver is
            when Adam_Solver => null;
            when Lbfgs_Solver => null;
            when Sgd_Solver => null;
         end case;
      end if;
   end Fit_Stochastic;

   --  -------------------------------------------------------------------------
   --  L320  BaseMultilayerPerceptron._Initialize
   procedure Init_Coeff (Self            : in out MLP_Classifier;
                         Fan_In, Fan_Out : Positive;
                         Coef_Init       : out IL_Types.Float_List_2D;
                         Intercept_Init  : out IL_Types.Float_List) is
      use Maths;
      use Float_Math_Functions;
      Factor         : Float;
      Init_Bound     : Float;
      Coef_Init_1    : IL_Types.Float_List;
   begin
      if Self.Parameters.Activation = Logistic_Activation then
         Factor := 2.0;
      else
         Factor := 6.0;
      end if;

      Init_Bound := Sqrt (Factor / Float (Fan_In + Fan_Out));
      --  Generate weights and bias
      Coef_Init.Clear;
      for f_in in 1 .. Fan_In loop
         Coef_Init_1.Clear;
         for f_out in 1 .. Fan_Out loop
            Coef_Init_1.Append (Init_Bound * Random_Float);
         end loop;
         Coef_Init.Append (Coef_Init_1);
      end loop;

      for index in 1 .. Fan_Out loop
         Intercept_Init.Append (Init_Bound * Random_Float);
      end loop;

   end Init_Coeff;

   --  -------------------------------------------------------------------------
 --  L417
   procedure Init_Coeff_Grads (Layer_Units     : IL_Types.Integer_List;
                               Coef_Grads      : out IL_Types.Float_List_3D;
                               Intercept_Grads : out IL_Types.Float_List_2D) is
      use Ada.Containers;
      use Utilities;
      use IL_Types;
      use Integer_Package;
      Fan_In_Units  : Integer_List;
      Fan_Out_Units : Integer_List;
      Intercept     : Float_List;

      function Build_List (Dims : Integer_Zip_Item) return Float_List_2D is
         Inner   : Float_List;
         theList : Float_List_2D;
      begin
         Inner.Set_Length (Count_Type (Dims.Integer_2));
         for index in 1 .. Dims.Integer_1 loop
            theList.Append (Inner);
         end loop;

         return theList;

      end Build_List;

      Zip_Layer_Units           : Integer_Zip_List;
   begin
      Fan_In_Units := Layer_Units;
      Integer_Package.Delete_Last (Fan_In_Units);
      Fan_Out_Units := Layer_Units;
      Integer_Package.Delete_First (Fan_In_Units);
      Zip_Layer_Units := Zip (Fan_In_Units, Fan_Out_Units);

      --  Coef_Grads is a 3D list of fan_in x fan_out lists
      for index in Zip_Layer_Units.First_Index ..
        Zip_Layer_Units.Last_Index loop
         Coef_Grads.Append (Build_List (Zip_Layer_Units.Element (index)));
      end loop;

      for index in Fan_Out_Units.First_Index ..
           Fan_Out_Units.Last_Index loop
         Intercept.Set_Length (Count_Type (Fan_Out_Units.Element (index)));
         Intercept_Grads.Append (Intercept);
      end loop;

   end Init_Coeff_Grads;

   --  -------------------------------------------------------------------------

   --  L320  BaseMultilayerPerceptron._Initialize
   procedure Initialize (Self        : in out MLP_Classifier;
                         Layer_Units : IL_Types.Integer_List) is
      use IL_Types;
--        Routine_Name : constant String := "Multilayer_Perceptron.Initialize ";
      Coef_Init      : Float_List_2D;
      Intercept_Init : Float_List;
   begin
      Self.Attributes.N_Iter := 0;
      Self.Attributes.T := 0;
      Self.Attributes.N_Layers := Natural (Layer_Units.Length);
      Self.Attributes.Out_Activation := Logistic_Activation;
      Self.Attributes.Coefs.Clear;
      Self.Attributes.Intercepts.Clear;

      for index in 1 .. Self.Attributes.N_Layers - 1 loop
         Init_Coeff
           (Self, Layer_Units.Element (index), Layer_Units.Element (index + 1),
            Coef_Init, Intercept_Init);
         Self.Attributes.Coefs.Append (Coef_Init);
         Self.Attributes.Intercepts.Append (Intercept_Init);
      end loop;

      if Self.Parameters.Solver = Sgd_Solver or else
        Self.Parameters.Solver = Adam_Solver then
         Self.Attributes.Loss_Curve.Clear;
         Self.Attributes.No_Improvement_Count := 0;
         if Self.Parameters.Early_Stopping then
            Self.Parameters.Validation_Scores.Clear;
            Self.Parameters.Best_Validation_Score := Float'Safe_First;
         else
            Self.Attributes.Best_Loss := Float'Safe_Last;
         end if;
      end if;

   end Initialize;

   --  -------------------------------------------------------------------------
   --  L455
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
