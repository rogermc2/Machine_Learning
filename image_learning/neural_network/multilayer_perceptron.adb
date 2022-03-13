--  Based on scikit-learn/sklearn/neural_network/_multilayer_perceptron.py
with Ada.Assertions; use Ada.Assertions;
with Ada.Containers;
with Ada.Text_IO; use Ada.Text_IO;

with Maths;
with Utilities;

with Data_Splitter;
with Encode_Utils;
with Label;
with Utils;

package body Multilayer_Perceptron is

   First_Pass : Boolean := True;

   procedure Fit_Stochastic (Self            : in out MLP_Classifier;
                             X               : IL_Types.Float_List_2D;
                             Y               : IL_Types.Integer_List;
                             Activations     : in out IL_Types.Float_List_2D;
                             Deltas          : IL_Types.Float_List;
                             Coef_Grads      : in out IL_Types.Float_List_3D;
                             Intercept_Grads : in out IL_Types.Float_List_2D;
                             Layer_Units     : IL_Types.Integer_List;
                             Incremental     : Boolean := False);
   procedure Forward_Pass (Self        : in out MLP_Classifier;
                           Activations : in out IL_Types.Float_List_2D);
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

   procedure Backprop (Self            : in out MLP_Classifier;
                       X               : IL_Types.Float_List_2D;
                       Y               : IL_Types.Integer_List;
                       Activations     : in out IL_Types.Float_List_2D;
                       Deltas          : IL_Types.Float_List;
                       Loss            : out Float;
                       Coef_Grads      : out IL_Types.Float_List_3D;
                       Intercept_Grads : out IL_Types.Float_List_2D) is
      Num_Samples      : constant Positive := Positive (X.Length);
   begin
      null;
   end Backprop;

   --  -------------------------------------------------------------------------

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
                             Activations     : in out IL_Types.Float_List_2D;
                             Deltas          : IL_Types.Float_List;
                             Coef_Grads      : in out IL_Types.Float_List_3D;
                             Intercept_Grads : in out IL_Types.Float_List_2D;
                             Layer_Units     : IL_Types.Integer_List;
                             Incremental     : Boolean := False) is

      use IL_Types;
      use List_Of_Float_Lists_Package;
      Routine_Name     : constant String :=
                           "Multilayer_Perceptron.Fit_Stochastic ";
      Num_Samples      : constant Positive := Positive (X.Length);
      LE_U             : Label.Label_Encoder (Label.Class_Unique);
      Params           : constant Float_List_3D :=
                           Self.Attributes.Coefs & Self.Attributes.Intercepts;
      Early_Stopping   : constant Boolean
        := Self.Parameters.Early_Stopping and then not Incremental;
      Test_Size        : constant Positive
        := Positive (Self.Parameters.Validation_Fraction * Float (Num_Samples));
      Train_Size       : constant Positive := Num_Samples - Test_Size;
      Stratify         : Integer_List;
      Should_Stratify  : Boolean;
      Train_X          : Float_List_2D;
      Train_Y          : Integer_List;
      Test_X           : Float_List_2D;
      Test_Y           : Integer_List;
      Batch_Size       : Positive;
      Sample_Index     : Positive;
      Max_Sample_Index : Positive;
      Accumulated_Loss : Float := 0.0;
      Batches          : Integer_List_2D;
      Batch_Slice      : Integer_List;
      X_Batch          : Float_List_2D;
      Y_Batch          : Integer_List;
      Loss             : Float;
   begin
      if not Incremental or else
        Self.Attributes.Optimizer.Kind = No_Optimizer then
         case Self.Parameters.Solver is
            when Adam_Solver =>
               declare
                  Optimizer : Optimizer_Record (Optimizer_Adam);
               begin
                  Optimizer.Adam.Params := Params;
                  Optimizer.Adam.Initial_Learning_Rate :=
                    Self.Parameters.Learning_Rate_Init;
                  Optimizer.Adam.Beta_1 := Self.Parameters.Beta_1;
                  Optimizer.Adam.Beta_2 := Self.Parameters.Beta_2;
                  Optimizer.Adam.Epsilon := Self.Parameters.Epsilon;
                  Self.Attributes.Optimizer := Optimizer;
               end;

            when Lbfgs_Solver =>
               declare
                  Optimizer : Optimizer_Record (Optimizer_SGD);
               begin
                  Optimizer.SGD.Params := Params;
                  Optimizer.SGD.Initial_Learning_Rate :=
                    Self.Parameters.Learning_Rate_Init;
                  Optimizer.SGD.Learning_Rate := Self.Parameters.Learning_Rate;
                  Optimizer.SGD.Momentum := Self.Parameters.Momentum;
                  Optimizer.SGD.Use_Nesterov :=
                    Self.Parameters.Nesterovs_Momentum;
                  Optimizer.SGD.Power_T := Self.Parameters.Power_T;
                  Self.Attributes.Optimizer := Optimizer;
               end;
            when Sgd_Solver => null;
         end case;
      end if;

      if Early_Stopping then
         Should_Stratify := Self.Parameters.Is_Classifier;
         if Should_Stratify then
            Stratify := Y;
         end if;

         Data_Splitter.Train_Test_Split
           (X => X, Y => Y,
            Train_Size => Train_Size, Test_Size  => Test_Size,
            Train_X => Train_X, Train_Y => Train_Y,
            Test_X  => Test_X, Test_Y => Test_Y);
         if Self.Parameters.Is_Classifier then
            Test_Y := Label.Inverse_Transform (LE_U, Test_Y);
         end if;
      end if;

      if Self.Parameters.Batch_Size = 0 then
         Batch_Size := Integer'Min (200, Num_Samples);
      else
         Put_Line (Routine_Name & "WARNING: Batch size " &
                     Integer'Image (Self.Parameters.Batch_Size)  &
                     "clipped to " & Integer'Image (Num_Samples));
         Batch_Size := Num_Samples;
      end if;

      Max_Sample_Index := Num_Samples;
      Batches := Utils.Gen_Batches (Num_Samples, Batch_Size);
      Activations.Clear;
      Sample_Index := 1;
      while Sample_Index <= Max_Sample_Index loop
         --  if Self.Parameters.Shuffle then
         --      Sample_Index := Shuffle (Sample_Index, Random_State);
         --  end if;
         Accumulated_Loss := 0.0;
         for batch_index in Batches.First_Index .. Batches.Last_Index loop
            Batch_Slice := Batches (batch_index);
            for index in Batch_Slice.First_Index .. Batch_Slice.Last_Index loop
               X_Batch (index) := X (Batch_Slice (index));
               Y_Batch (index) := Y_Batch (Batch_Slice (index));
            end loop;
            Activations.Append (X_Batch);
            Backprop (Self, X, Y, Activations, Deltas, Loss,
                      Coef_Grads, Intercept_Grads);

         end loop;

         Sample_Index := Sample_Index + 1;
      end loop;

   end Fit_Stochastic;

   --  -------------------------------------------------------------------------

   procedure Forward_Pass (Self        : in out MLP_Classifier;
                           Activations : in out IL_Types.Float_List_2D) is
   begin
      null;
   end Forward_Pass;

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
