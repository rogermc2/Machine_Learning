--  Based on scikit-learn/sklearn/neural_network/_multilayer_perceptron.py

--  The simplest representation of a neural network is a Multilayer Perceptron.
--  In its simplest form a MLP is just three layers.
--  An input layer represented by a real matrix X (N×d) where N is the number
--  of training examples and d is the number of features.
--  A hidden layer which is usually a ReLU or a logistic sigmoid function.
--  Hidden layer i could be a ReLU function represented by
--  h_i(x) = ReLU (x) = max (x, 0)
--  In other words, if the input to the ReLU function is negative, the function
--  outputs a 0.
--  If the inputs x are positive, the ReLU function will output x.
--  The hidden layer feeds into the output layer which is just another function.
--  This function could be squared error function (in the context of regression)
--  or softmax (in the case of multiclass classification).
--  The MLP is complete when you consider the weight and bias matrices but
--  we don't need them for now.
--  The activation function is just what the name suggests; a function.
--  In the example above, the activation function for the hidden layer is the
--  ReLU function.
--  The activation function for the output layer was squared error or softmax.
--  the word "activations" in Machine Learning almost always refers to the
--- output of the activation function.
--  The possible activations in the hidden layer in the example above could
--  only be either 0 or 1.
--  Note that the hidden activations (output from the hidden layer) could
--  become input to other activation functions (in this case, the output layer
--  activation functions).
--  Pre-activation means the input to an activation function.

--  A Multilayer_Perceptron contains the attributes Coefs and Intercepts.
--  Coefs is a 3D list of weight matrices where the weight matrix at index i
--  represents the weights between layer i and layer i + 1.
--  Intercepts is a 2D list of bias vectors where the vector at index
--  the bias values added to layer i + 1.

with Ada.Assertions; use Ada.Assertions;
with Ada.Containers;
with Ada.Text_IO; use Ada.Text_IO;

with Maths;
with Utilities;

with Classifier_Utilities;
with Data_Splitter;
with Encode_Utils;
with Label;
with Utils;

package body Multilayer_Perceptron is
   pragma Warnings (Off);

   First_Pass : Boolean := True;

   procedure Compute_Loss_Gradient
     (Self            : in out MLP_Classifier;
      Layer           : Positive;
      Num_Samples     : Positive;
      Activations     : IL_Types.Float_List_2D;
      Deltas          : IL_Types.Float_List_2D;
      Coef_Grads      : in out IL_Types.Float_List_3D;
      Intercept_Grads : in out IL_Types.Float_List_2D);
   procedure Fit_Lbfgs (Self            : in out MLP_Classifier;
                        X               : IL_Types.Float_List_2D;
                        Y               : IL_Types.Integer_List;
                        Activations     : in out IL_Types.Float_List_2D;
                        Deltas          : in out IL_Types.Float_List_2D;
                        Coef_Grads      : in out IL_Types.Float_List_3D;
                        Intercept_Grads : in out IL_Types.Float_List_2D;
                        Layer_Units     : IL_Types.Integer_List);
   procedure Fit_Stochastic (Self            : in out MLP_Classifier;
                             X               : IL_Types.Float_List_2D;
                             Y               : IL_Types.Integer_List;
                             Activations     : in out IL_Types.Float_List_2D;
                             Deltas          : in out IL_Types.Float_List_2D;
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
   procedure Update_No_Improvement_Count (Self : in out MLP_Classifier;
                                          Early_Stopping : Boolean;
                                          X_Val, Y_Val : Float);
   procedure Validate_Hyperparameters (Self : MLP_Classifier);
   procedure Validate_Input (Self               : in out MLP_Classifier;
                             --                               X                  : IL_Types.Float_List_2D;
                             Y                  : IL_Types.Integer_List);
   --                               Incremental, Reset : Boolean);

   --  -------------------------------------------------------------------------
   --  L241
   procedure Backprop (Self            : in out MLP_Classifier;
                       X               : IL_Types.Float_List_2D;
                       Y               : IL_Types.Integer_List;
                       Activations     : in out IL_Types.Float_List_2D;
                       Deltas          : in out IL_Types.Float_List_2D;
                       Loss            : out Float;
                       Coef_Grads      : out IL_Types.Float_List_3D;
                       Intercept_Grads : out IL_Types.Float_List_2D) is
      use Base;
      use IL_Types;
      use Float_List_Package;
      use Float_Package;
      Routine_Name       : constant String := "Multilayer_Perceptron.Backprop ";
      Num_Samples        : constant Positive := Positive (X.Length);
      Loss_Function_Name : Loss_Function;
      S_List             : Float_List_2D;
      Ravel              : Float_List;
      Values             : Float := 0.0;
      F_I                : Positive;
      Last               : Positive;
      Activation         : Float_List;
      Diff               : Float_List;
      Derivative_Kind    : Derivative_Type;
      Inplace_Derivative : Float_List;
   begin
      Forward_Pass (Self, Activations);
      if Self.Attributes.Loss_Function_Name = Log_Loss_Function and then
        Self.Attributes.Out_Activation = Logistic_Activation then
         Loss_Function_Name := Binary_Log_Loss_Function;
      end if;

      case Loss_Function_Name is
         when Binary_Log_Loss_Function =>
            Loss := Binary_Log_Loss (Y, Activations.Last_Element);
         when Log_Loss_Function =>
            Loss := Log_Loss (Y, Activations.Last_Element);
         when Squared_Error_Function =>
            Loss := Squared_Error (Y, Activations.Last_Element);
      end case;

      for s in Self.Attributes.Neuron_Coef_Layers.First_Index ..
        Self.Attributes.Neuron_Coef_Layers.Last_Index loop
         S_List := Self.Attributes.Neuron_Coef_Layers.Element (s);
         for s_index in S_List.First_Index .. S_List.Last_Index loop
            Ravel := Ravel & S_List.Element (s_index);
         end loop;
         Values := Values + Dot (Ravel, Ravel);
      end loop;

      --  Add L2 regularization term to loss
      Loss := Loss + 0.5 * Self.Parameters.Alpha * Values / Float (Num_Samples);

      --  L294 Backward propagate
      --  The calculation of delta[last]  works with following combinations
      --  of output activation and loss function:
      --  sigmoid and binary cross entropy, softmax and categorical cross
      --  entropy and identity with squared loss.
      --  deltas : list, length = n_layers - 1
      --  The ith element of deltas holds the difference between the activations
      --  of the i + 1 layer and the backpropagated error.
      --  Deltas are gradients of loss with respect to z in each layer.
      --  The ith element of Activations (layers x values) is a list of values
      --  of the ith layer.

      Last := Num_Samples - 1;
      Activation := Activations.Element (Activations.Last_Index - 1);
      Diff := Activation - Classifier_Utilities.To_Float_List (Y);
      Put_Line (Routine_Name & "Last" & Integer'Image (Last));
      Put_Line (Routine_Name & "Deltas Last_Index" & Integer'Image (Deltas.Last_Index));
      --        Deltas.Replace_Element (Last, Diff);

      --  L303  Compute gradient for the last layer
      Compute_Loss_Gradient (Self, Last, Num_Samples, Activations, Deltas,
                             Coef_Grads, Intercept_Grads);

      --  L308, L309
      for index in reverse 2 .. Self.Attributes.N_Layers - 1 loop
         Deltas (index - 1) :=
           Dot (Deltas (index),
                Transpose (Self.Attributes.Neuron_Coef_Layers (index)));
         case Self.Parameters.Activation is
            when Identity_Activation =>
               Identity_Derivative (Activations (index), Deltas (index - 1));
            when Logistic_Activation =>
               Logistic_Derivative (Activations (index), Deltas (index - 1));
            when Tanh_Activation =>
               Tanh_Derivative (Activations (index), Deltas (index - 1));
            when Relu_Activation =>
               Relu_Derivative (Activations (index), Deltas (index - 1));
            when Softmax_Activation => null;
         end case;

         Compute_Loss_Gradient (Self, index - 1, Num_Samples, Activations,
                                Deltas, Coef_Grads, Intercept_Grads);
      end loop;

   end Backprop;

   --  -------------------------------------------------------------------------

   function C_Init (Hidden_Layer_Sizes  : IL_Types.Integer_List :=
                      IL_Types.Integer_Package.Empty_Vector;
                    Activation          : Base.Activation_Type :=
                      Base.Relu_Activation;
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
   --  L177 Compute_Loss_Gradient does back-propagation for a specified layer
   --  by computing the gradient of loss with respect to the coefs and
   --  intercept for the layer.

   --  Coef_Grads is a 3D list of weight matrices where the weight matrix at index i
   --  represents the weights between layer i and layer i + 1.
   --  Intercept_Grads is a 2D list of bias vectors where the vector at index
   --  the bias values added to layer i + 1.
   procedure Compute_Loss_Gradient
     (Self            : in out MLP_Classifier;
      Layer           : Positive;
      Num_Samples     : Positive;
      Activations     : IL_Types.Float_List_2D;
      Deltas          : IL_Types.Float_List_2D;
      Coef_Grads      : in out IL_Types.Float_List_3D;
      Intercept_Grads : in out IL_Types.Float_List_2D) is
      use IL_Types;
      use Float_List_Package;
      use Float_Package;
      Delta_Act  : constant Float_List_2D :=
                     Dot (Deltas (Layer), Activations (Layer));
      Delta_Mean : constant Float := Utilities.Mean (Deltas (Layer));
   begin
      --  Coef_Grads is a 3D list of fan_in x fan_out lists
      Coef_Grads (Layer) :=
        (Delta_Act + Self.Parameters.Alpha *
           Self.Attributes.Neuron_Coef_Layers (Layer)) / Float (Num_Samples);

      --  Intercept_Grads is 2D layer x fan_out
      --  The ith element of Deltas holds the difference between the
      --  activations of the i + 1 layer and the backpropagated error.

      for index in Intercept_Grads (Layer).First_Index ..
        Intercept_Grads (Layer).Last_Index loop
         Intercept_Grads (Layer) (index) := Delta_Mean;
      end loop;

   end  Compute_Loss_Gradient;

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
      Activations               : Float_List_2D := X;
      Hidden_Layer_Sizes        : Integer_List :=
                                    Self.Parameters.Hidden_Layer_Sizes;
      Hidden_Layer_Sizes_Length : Count_Type := Hidden_Layer_Sizes.Length;
      Layer_Units               : Integer_List;
      Deltas                    : Float_List_2D;
      Coef_Grads                : Float_List_3D;
      Intercept_Grads           : Float_List_2D;
   begin
      Validate_Hyperparameters (Self);
      First_Pass :=
        Self.Attributes.Neuron_Coef_Layers.Is_Empty or else
        (not Self.Parameters.Warm_Start and then not Incremental);

      Layer_Units.Append (Num_Features);
      for index in Hidden_Layer_Sizes.First_Index ..
        Hidden_Layer_Sizes.Last_Index loop
         Layer_Units.Append (Hidden_Layer_Sizes.Element (index));
      end loop;

      Validate_Input (Self, Y);

      --  L409
      if First_Pass then
         Initialize (Self, Layer_Units);
      end if;

      Activations.Set_Length (X.Length + Layer_Units.Length);
      --  Deltas is a 2D list initialized by Backprop
      --  The ith element of Deltas holds the difference between the
      --  activations of the i + 1 layer and the backpropagated error.
      Deltas.Set_Length (Activations.Length - 1);
      --  L417
      Init_Coeff_Grads (Layer_Units, Coef_Grads, Intercept_Grads);

      --  L427
      if Self.Parameters.Solver = Sgd_Solver or else
        Self.Parameters.Solver = Adam_Solver then
         Fit_Stochastic (Self, X, Y, Activations, Deltas, Coef_Grads,
                         Intercept_Grads, Layer_Units, Incremental);
      elsif Self.Parameters.Solver = Lbfgs_Solver then
         Fit_Lbfgs (Self, X, Y, Activations, Deltas, Coef_Grads,
                    Intercept_Grads, Layer_Units);
      end if;

   end Fit;

   --  -------------------------------------------------------------------------
   --  L516
   procedure Fit_Lbfgs (Self            : in out MLP_Classifier;
                        X               : IL_Types.Float_List_2D;
                        Y               : IL_Types.Integer_List;
                        Activations     : in out IL_Types.Float_List_2D;
                        Deltas          : in out IL_Types.Float_List_2D;
                        Coef_Grads      : in out IL_Types.Float_List_3D;
                        Intercept_Grads : in out IL_Types.Float_List_2D;
                        Layer_Units     : IL_Types.Integer_List) is
      use IL_Types;
      use List_Of_Float_Lists_Package;
      Routine_Name : constant String := "Multilayer_Perceptron.Fit_Lbfgs ";
      Num_Samples  : constant Positive := Positive (X.Length);
      Start        : Positive := 1;
      Last         : Positive;
      N_Fan_In     : Positive;
      N_Fan_Out    : Positive;
   begin
      Self.Attributes.Coef_Indptr.Clear;
      Self.Attributes.Intercept_Indptr.Clear;

      --  L524  Save sizes and indices of coefficients for faster unpacking
      for index in 1 .. Self.Attributes.N_Layers - 1 loop
         N_Fan_In := Layer_Units (index);
         N_Fan_Out := Layer_Units (index + 1);
         Last := Start + N_Fan_In * N_Fan_Out;
         Self.Attributes.Coef_Indptr.Append ((Start, Last,
                                             N_Fan_In, N_Fan_Out));
         Start := Last + 1;
      end loop;

      --  L532  Save sizes and indices of intercepts for faster unpacking
      Start := 1;
      for index in 1 .. Self.Attributes.N_Layers - 1 loop
         Last := Start + N_Fan_In * N_Fan_Out;
         Self.Attributes.Intercept_Indptr.Append ((Start, Last));
         Start := Last + 1;
      end loop;

      Assert (False, Routine_Name & "coding incomplete.");

   end Fit_Lbfgs;

   --  -------------------------------------------------------------------------

   --  L563
   procedure Fit_Stochastic (Self            : in out MLP_Classifier;
                             X               : IL_Types.Float_List_2D;
                             Y               : IL_Types.Integer_List;
                             Activations     : in out IL_Types.Float_List_2D;
                             Deltas          : in out IL_Types.Float_List_2D;
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
                           Self.Attributes.Neuron_Coef_Layers &
                           Self.Attributes.Intercepts;
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
      Batch_Loss       : Float;
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

      --  L597
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

      --  L617
      if Self.Parameters.Batch_Size = 0 then
         Batch_Size := Integer'Min (200, Num_Samples);
      else
         if Self.Parameters.Batch_Size > Num_Samples then
            Put_Line (Routine_Name & "WARNING: Batch size" &
                        Integer'Image (Self.Parameters.Batch_Size)  &
                        " clipped to" & Integer'Image (Num_Samples));
         end if;
         Batch_Size := Num_Samples;
      end if;

      --        Max_Sample_Index := Num_Samples;
      --  L628
      for iter in 1 .. Self.Parameters.Max_Iter loop
         Accumulated_Loss := 0.0;
         Batches := Utils.Gen_Batches (Num_Samples, Batch_Size);
         Put_Line (Routine_Name & "batches generated");
         Activations.Clear;
         --           Sample_Index := 1;
         --        while Sample_Index <= Max_Sample_Index loop
         --  if Self.Parameters.Shuffle then
         --      Sample_Index := Shuffle (Sample_Index, Random_State);
         --  end if;
         --  L636
         for batch_index in Batches.First_Index .. Batches.Last_Index loop
            Batch_Slice := Batches (batch_index);
            X_Batch.Clear;
            Y_Batch.Clear;
            Put_Line (Routine_Name & "batch_index:" &
                        Integer'Image (batch_index));
            Put_Line (Routine_Name & "X_Batch size:" &
                        Integer'Image (Integer (X_Batch.Length)));
            for index in Batch_Slice.First_Index .. Batch_Slice.Last_Index loop
               Put_Line (Routine_Name & "Batch_Slice index:" &
                           Integer'Image (index));
               X_Batch.Append (X (Batch_Slice (index)));
               Y_Batch.Append (Y_Batch (Batch_Slice (index)));
               Activations.Append (X_Batch);

               --  L645
               Backprop (Self, X, Y, Activations, Deltas, Batch_Loss,
                      Coef_Grads, Intercept_Grads);
               Accumulated_Loss := Accumulated_Loss + Batch_Loss *
                 Float (Batch_Slice.Last_Index - Batch_Slice.First_Index + 1);

               --  L657 update weights

            end loop;

            --  L661
            Self.Attributes.N_Iter := Self.Attributes.N_Iter + 1;
            --           Sample_Index := Sample_Index + 1;
            Self.Attributes.Loss := Accumulated_Loss / Float (Num_Samples);
            Self.Attributes.T := Self.Attributes.T + Num_Samples;
            Self.Attributes.Loss_Curve.Append (Self.Attributes.Loss);
            if Self.Parameters.Verbose then
               Put_Line (Routine_Name & "Iteration" &
                           Integer'Image (Self.Attributes.N_Iter) &
                           "loss = " & Float'Image (Self.Attributes.Loss));
            end if;

            --  L671

         end loop;
      end loop;

   end Fit_Stochastic;

   --  -------------------------------------------------------------------------
   --  L119
   procedure Forward_Pass (Self        : in out MLP_Classifier;
                           Activations : in out IL_Types.Float_List_2D) is
      --  The ith element of Activations holds the values of the ith layer.
      use Base;
      use IL_Types;
      use Float_Package;
      use Float_List_Package;
      Hidden_Activation  : constant Activation_Type :=
                             Self.Parameters.Activation;
      Output_Activation  : constant Activation_Type :=
                             Self.Attributes.Out_Activation;
      Last_Hidden_Index  : constant Positive := Self.Attributes.N_Layers - 1;
      Coefficient_Matrix : Float_List_2D;
   begin
      --  Iterate over the hidden layers
      for index in 1 .. Last_Hidden_Index loop
         Coefficient_Matrix := Self.Attributes.Neuron_Coef_Layers (index);
         Activations (index + 1) := Dot
           (Activations (index), Coefficient_Matrix);
         Activations (index + 1) := Activations (index + 1) &
           Self.Attributes.Intercepts (index);

         --  For the hidden layers
         if index + 1 /= Last_Hidden_Index then
            case Hidden_Activation is
               when Identity_Activation =>
                  Activations (index + 1) := Activations (index);
               when Logistic_Activation =>
                  Activations (index + 1) := Logistic (Activations (index));
               when Tanh_Activation =>
                  Activations (index + 1) := Tanh (Activations (index));
               when Relu_Activation =>
                  Activations (index + 1) := Relu (Activations (index));
               when Softmax_Activation =>
                  Activations (index + 1) := Softmax (Activations (index));
            end case;
         end if;
      end loop;

      --  For the last layer
      case Output_Activation is
         when Identity_Activation =>
            Activations.Replace_Element
              (Activations.Last_Index, Activations.Element (Last_Hidden_Index));
         when Logistic_Activation => Activations.Replace_Element
              (Activations.Last_Index,
               Logistic (Activations (Last_Hidden_Index)));
         when Tanh_Activation => Activations.Replace_Element
              (Activations.Last_Index, Tanh (Activations (Last_Hidden_Index)));
         when Relu_Activation => Activations.Replace_Element
              (Activations.Last_Index, Relu (Activations (Last_Hidden_Index)));
         when Softmax_Activation => Activations.Replace_Element
              (Activations.Last_Index,
               Softmax (Activations (Last_Hidden_Index)));
      end case;

   end Forward_Pass;

   --  -------------------------------------------------------------------------

   --  L320  BaseMultilayerPerceptron._Initialize
   procedure Init_Coeff (Self            : in out MLP_Classifier;
                         Fan_In, Fan_Out : Positive;
                         Coef_Init       : out IL_Types.Float_List_2D;
                         Intercept_Init  : out IL_Types.Float_List) is
      use Maths;
      use Float_Math_Functions;
      use Base;
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

      --  Intercept_Grads is a 2D list of fan_out lists
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
      use Base;
      --        Routine_Name : constant String := "Multilayer_Perceptron.Initialize ";
      Coef_Init      : Float_List_2D;
      Intercept_Init : Float_List;
   begin
      Self.Attributes.N_Iter := 0;
      Self.Attributes.T := 0;
      Self.Attributes.N_Layers := Natural (Layer_Units.Length);
      Self.Attributes.Out_Activation := Logistic_Activation;
      Self.Attributes.Neuron_Coef_Layers.Clear;
      Self.Attributes.Intercepts.Clear;

      for index in 1 .. Self.Attributes.N_Layers - 1 loop
         Init_Coeff
           (Self, Layer_Units.Element (index), Layer_Units.Element (index + 1),
            Coef_Init, Intercept_Init);
         Self.Attributes.Neuron_Coef_Layers.Append (Coef_Init);
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
   --  L716
   procedure Update_No_Improvement_Count
      (Self : in out MLP_Classifier; Early_Stopping : Boolean;
       X_Val, Y_Val : Float) is
      Score : Float;
   begin
      if Early_Stopping then
         Score := Base.Score (X_Val, Y_Val);
        Self.Parameters.Validation_Scores.Append
           (Score);
      else
         null;
      end if;

   end Update_No_Improvement_Count;

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
