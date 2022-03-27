--  Based on scikit-learn/sklearn/neural_network/_multilayer_perceptron.py

--  The simplest representation of a neural network is a Multilayer Perceptron.
--  In its simplest form a MLP is just three layers.
--  An input layer represented by a real matrix X (N�d) where N is the number
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
--  with Utilities;

with Base_Mix;
with Classifier_Utilities;
with Data_Splitter2;
with Encode_Utils;
with Label;
with Neural_Maths;
with Printing;
with Utils;

package body Multilayer_Perceptron is
   pragma Warnings (Off);

   First_Pass : Boolean := True;

   procedure Compute_Loss_Gradient
     (Self            : in out MLP_Classifier;
      Layer           : Positive;
      Num_Samples     : Positive;
      Activations     : Float_List_3D;
      Deltas          : Float_List_3D;
      Coef_Grads      : in out Float_List_3D;
      Intercept_Grads : in out Float_List_2D);
   procedure Fit_Lbfgs (Self            : in out MLP_Classifier;
                        X               : Float_List_2D;
                        Y               : Integer_List_2D;
                        Activations     : in out Float_List_3D;
                        Deltas          : in out Float_List_3D;
                        Coef_Grads      : in out Float_List_3D;
                        Intercept_Grads : in out Float_List_2D;
                        Layer_Units     : Integer_List);
   procedure Fit_Stochastic (Self            : in out MLP_Classifier;
                             X               : Float_List_2D;
                             Y               : Integer_List_2D;
                             Activations     : in out Float_List_3D;
                             Deltas          : in out Float_List_3D;
                             Coef_Grads      : in out Float_List_3D;
                             Intercept_Grads : in out Float_List_2D;
                             Layer_Units     : Integer_List;
                             Incremental     : Boolean := False);
   procedure Forward_Pass (Self            : in out MLP_Classifier;
                           Activations     : in out Float_List_3D);
   procedure Initialize (Self        : in out MLP_Classifier;
                         Layer_Units : Integer_List);
   procedure Init_Coeff (Self            : in out MLP_Classifier;
                         Fan_In, Fan_Out : Positive;
                         Coef_Init       : out Float_List_2D;
                         Intercept_Init  : out Float_List);
   procedure Update_No_Improvement_Count
     (Self  : in out MLP_Classifier; Early_Stopping : Boolean;
      X_Val : Float_List_2D);
   procedure Validate_Hyperparameters (Self : MLP_Classifier);
   procedure Validate_Input (Self               : in out MLP_Classifier;
                             Y                  : Integer_List);
   --                               Incremental, Reset : Boolean);

   --  -------------------------------------------------------------------------
   --  L241  Backprop computes the MLP loss function and its derivatives
   --       with respect to each parameter: weights and bias vectors.
   procedure Backprop (Self            : in out MLP_Classifier;
                       X               : Float_List_2D;
                       Y               : Integer_List_2D;
                       Activations     : in out Float_List_3D;
                       Deltas          : in out Float_List_3D;
                       Loss            : out Float;
                       Coef_Grads      : out Float_List_3D;
                       Intercept_Grads : out Float_List_2D) is
      use Ada.Containers;
      use Base_Neural;
      use Float_Package;
      Routine_Name       : constant String := "Multilayer_Perceptron.Backprop ";
      Y_Float            : constant Float_List_2D :=
                             Classifier_Utilities.To_Float_List_2D (Y);
      Num_Samples        : constant Positive := Positive (X.Length);
      Loss_Function_Name : Loss_Function;
      S_List             : Float_List_2D;
      Ravel              : Float_List;
      Values             : Float := 0.0;
      F_I                : Positive;
      Last               : Positive;
      Activation         : Float_List_2D;
      Derivative_Kind    : Derivative_Type;
      Inplace_Derivative : Float_List;
   begin
--        Put_Line (Routine_Name & "Pre Forward_Pass, Activations size:" &
--                    Count_Type'Image (Activations.Length));
--        Put_Line (Routine_Name & "Pre Forward_Pass, Activations size:" &
--                    Count_Type'Image (Activations (1).Length) & " x" &
--                    Count_Type'Image (Activations (1) (1).Length));

      Forward_Pass (Self, Activations);
      --  L284
      if Self.Attributes.Loss_Function_Name = Log_Loss_Function and then
        Self.Attributes.Out_Activation = Logistic_Activation then
         Loss_Function_Name := Binary_Log_Loss_Function;
      end if;

      --        Printing.Print_Float_Lists_2D
      --        (Routine_Name & "Activations last layer", Activations.Last_Element);
      Assert (Y.Length = Activations.Last_Element.Length, Routine_Name &
                "L284 Y Length" & Count_Type'Image (Y.Length) &
                " should be the same as last activation length" &
                Count_Type'Image (Activations.Last_Element.Length));

      case Loss_Function_Name is
         when Binary_Log_Loss_Function =>
            Loss := Binary_Log_Loss (Y, Activations.Last_Element);
         when Log_Loss_Function =>
            Loss := Log_Loss (Y, Activations.Last_Element);
         when Squared_Error_Function =>
            Loss := Squared_Loss (Y, Activations.Last_Element);
      end case;

      --  L289  Add L2 regularization term to loss
      for s in Self.Attributes.Neuron_Coef_Layers.First_Index ..
        Self.Attributes.Neuron_Coef_Layers.Last_Index loop
         S_List := Self.Attributes.Neuron_Coef_Layers.Element (s);
         for s_index in S_List.First_Index .. S_List.Last_Index loop
            Ravel := Ravel & S_List.Element (s_index);
         end loop;
         Values := Values + Dot (Ravel, Ravel);
      end loop;
      Loss := Loss + 0.5 * Self.Parameters.Alpha * Values / Float (Num_Samples);
      --        Put_Line (Routine_Name & "loss + L2 regularization : " &
      --                    Float'Image (Loss));

      --  L297 Backward propagate
      --  The calculation of delta[last]  works with the following combinations
      --  of output activation and loss function:
      --  sigmoid and binary cross entropy, softmax and categorical cross
      --  entropy and identity with squared loss.
      --  deltas : list, length = n_layers - 1
      --  The ith element of deltas holds the difference between the activations
      --  of the i + 1 layer and the backpropagated error.
      --  Deltas are gradients of loss with respect to z in each layer.
      --  The ith element of Activations (layers x values) is a list of values
      --  of the ith layer.

      --  L295  Backward propagate python last = self.n_layers_ - 2
      Last := Self.Attributes.N_Layers - 1;
      --  L301
      Activation := Activations.Last_Element;
      Assert (Last = Natural (Deltas.Length), Routine_Name & "L301 Last" &
                Integer'Image (Last) & " should equal Deltas length" &
                Count_Type'Image (Deltas.Length));

      Assert (Y_Float.Length = Activations.Last_Element.Length, Routine_Name &
                "L301 Y_Float length" &
                Count_Type'Image (Y_Float.Length) &
                " should equal Activations.Last_Element length" &
                Count_Type'Image (Activations.Last_Element.Length));
      Deltas (Last) := Activations.Last_Element - Y_Float;

      --  L304  Compute gradient for the last layer
      Compute_Loss_Gradient (Self, Last, Num_Samples, Activations, Deltas,
                             Coef_Grads, Intercept_Grads);

      --  L310, L308
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
   --  L1054
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
      --        Activations     : Activation_List;
      Activations     : Float_List_3D;
      Deltas          : Float_List_3D;
      Coef_Grads      : in out Float_List_3D;
      Intercept_Grads : in out Float_List_2D) is
      use Ada.Containers;
      use Float_List_Package;
      use Float_Package;
--        Routine_Name : constant String :=
--                         "Multilayer_Perceptron.Compute_Loss_Gradient ";
      Delta_Act    : Float_List_2D;
      Delta_Mean   : Float_List;
   begin
--        Put_Line (Routine_Name & "Deltas (Layer) length" &
--                    Integer'Image (Integer (Deltas (Layer).Length)));
--        Put_Line (Routine_Name & "Activations (Layer) length" &
--                    Integer'Image (Integer (Activations (Layer).Length)));
      Delta_Act := Dot (Deltas (Layer), Activations (Layer));
      Delta_Mean := Neural_Maths.Mean (Deltas (Layer), 1);

      --  Coef_Grads is a 3D list of fan_in x fan_out lists
      if Coef_Grads.Is_Empty or else Positive (Coef_Grads.Length) < Layer then
         Coef_Grads.Set_Length (Count_Type (Layer));
      end if;
      Coef_Grads (Layer) := (Delta_Act +
        Self.Parameters.Alpha * Self.Attributes.Neuron_Coef_Layers (Layer));
      Coef_Grads (Layer) := Coef_Grads (Layer) / Float (Num_Samples);

      --  Intercept_Grads is 2D layer x fan_out
      --  The ith element of Deltas holds the difference between the
      --  activations of the i + 1 layer and the backpropagated error.

      if Intercept_Grads.Is_Empty or else
        Positive (Intercept_Grads.Length) < Layer then
         Intercept_Grads.Set_Length (Count_Type (Layer));
      end if;

      for index in Intercept_Grads (Layer).First_Index ..
        Intercept_Grads (Layer).Last_Index loop
         Intercept_Grads (Layer) := Delta_Mean;
      end loop;

   end  Compute_Loss_Gradient;

   --  -------------------------------------------------------------------------
   --  L377  BaseMultilayerPerceptron._Fit
   procedure Fit (Self        : in out MLP_Classifier;
                  X           : Float_List_2D;
                  Y           : Integer_List;
                  Incremental : Boolean := False) is
      use Ada.Containers;
      Routine_Name              : constant String :=
                                    "Multilayer_Perceptron.Fit ";
      --        Num_Samples               : constant Positive := Positive (X.Length);
      Num_Features              : constant Positive :=
                                    Positive (X.Element (1).Length);
      --  The ith element of Activations holds the values of the ith layer.
      Activations               : Float_List_3D;
      Y_2D                      : Integer_List_2D;
      Y_Col                     : Integer_List;
      Hidden_Layer_Sizes        : Integer_List :=
                                    Self.Parameters.Hidden_Layer_Sizes;
      Hidden_Layer_Sizes_Length : Count_Type := Hidden_Layer_Sizes.Length;
      Layer_Units               : Integer_List;
      Deltas                    : Float_List_3D;
      --  Coef_Grads layers * features * values
      Coef_Grads                : Float_List_3D;
      --  Coef_Grads layers * y values
      Intercept_Grads           : Float_List_2D;
   begin
      Validate_Hyperparameters (Self);
      First_Pass :=
        Self.Attributes.Neuron_Coef_Layers.Is_Empty or else
        (not Self.Parameters.Warm_Start and then not Incremental);

      --  L398  Ensure y is 2D
      for row in Y.First_Index .. Y.Last_Index loop
         Y_Col.Clear;
         Y_Col.Append (Y (row));
         Y_2D.Append (Y_Col);
      end loop;

      --  L404
      --  layer_units = [n_features] + hidden_layer_sizes + [self.n_outputs_]
      Layer_Units.Append (Num_Features);
      if Hidden_Layer_Sizes.Length > 0 then
         for index in Hidden_Layer_Sizes.First_Index ..
           Hidden_Layer_Sizes.Last_Index loop
            Layer_Units.Append (Hidden_Layer_Sizes.Element (index));
         end loop;
      end if;
      Layer_Units.Append (Self.Attributes.N_Outputs);

      --  L409
      if First_Pass then
         Initialize (Self, Layer_Units);
      end if;

      --  L414 Set the Activation values of the first layer
      Activations.Append (X);  -- layer x samples x features
      --  Deltas is a 3D list initialized by Backprop
      --  The ith element of Deltas holds the difference between the
      --  activations of the i + 1 layer and the backpropagated error.
      Deltas.Set_Length (Layer_Units.Length - 1);

      --  L417 Initialized grads are empty vectors, no initialization required.

      --  L427
      if Self.Parameters.Solver = Sgd_Solver or else
        Self.Parameters.Solver = Adam_Solver then
         Fit_Stochastic (Self, X, Y_2D, Activations, Deltas, Coef_Grads,
                         Intercept_Grads, Layer_Units, Incremental);
      elsif Self.Parameters.Solver = Lbfgs_Solver then
         Fit_Lbfgs (Self, X, Y_2D, Activations, Deltas, Coef_Grads,
                    Intercept_Grads, Layer_Units);
      end if;

   end Fit;

   --  -------------------------------------------------------------------------
   --  L516
   procedure Fit_Lbfgs (Self            : in out MLP_Classifier;
                        X               : Float_List_2D;
                        Y               : Integer_List_2D;
                        Activations     : in out Float_List_3D;
                        Deltas          : in out Float_List_3D;
                        Coef_Grads      : in out Float_List_3D;
                        Intercept_Grads : in out Float_List_2D;
                        Layer_Units     : Integer_List) is
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
         N_Fan_In := Layer_Units.Element (index);
         N_Fan_Out := Layer_Units.Element (index + 1);
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
                             X               : Float_List_2D;
                             Y               : Integer_List_2D;
                             Activations     : in out Float_List_3D;
                             Deltas          : in out Float_List_3D;
                             Coef_Grads      : in out Float_List_3D;
                             Intercept_Grads : in out Float_List_2D;
                             Layer_Units     : Integer_List;
                             Incremental     : Boolean := False) is
      use Ada.Containers;
      use Estimator;
      use List_Of_Float_Lists_Package;
      Routine_Name       : constant String :=
                             "Multilayer_Perceptron.Fit_Stochastic ";
      Is_Classifier      : constant Boolean :=
                             Self.Estimator_Kind = Classifier_Estimator;
      Num_Samples        : constant Positive := Positive (X.Length);
      LE_U               : Label.Label_Encoder (Label.Class_Unique);
      --  Activations: layers x samples x features
      --  Coeff_Params: layers x features x values
      --  The ith element of Coeff_Params represents the weight matrix
      --  corresponding to layer i.
      Coeff_Params       : constant Float_List_3D :=
                             Self.Attributes.Neuron_Coef_Layers;
      --  Intercept_Params: Layers x y values
      --  The ith element of Intercept_Params represents the bias vector
      --  corresponding to layer i + 1.
      Intercept_Params   : constant Float_List_2D := Self.Attributes.Intercepts;
      Early_Stopping     : constant Boolean
        := Self.Parameters.Early_Stopping and then not Incremental;
      Test_Size          : constant Positive
        := Positive (Self.Parameters.Validation_Fraction * Float (Num_Samples));
      Train_Size         : constant Positive := Num_Samples - Test_Size;
      Stratify           : Integer_List_2D;
      Should_Stratify    : Boolean;
      Train_X            : Float_List_2D;
      Train_Y            : Integer_List_2D;
      Test_X             : Float_List_2D;
      Test_Y             : Integer_List_2D;
      Batch_Size         : Positive;
      Sample_Index       : Positive;
      Max_Sample_Index   : Positive;
      Accumulated_Loss   : Float := 0.0;
      Batches            : Slices_List;
      Batch_Slice        : Slice_Record;
      X_Batch            : Float_List_2D;
      Y_Batch            : Integer_List_2D;
      Batch_Loss         : Float;
      Parameters         : Parameters_Record;
      Grads              : Parameters_Record;
   begin
      Parameters := (Coeff_Params, Intercept_Params);
      if not Incremental or else
        Self.Attributes.Optimizer.Kind = No_Optimizer then
         case Self.Parameters.Solver is
            when Adam_Solver =>
               declare
                  Optimizer : Optimizer_Record (Optimizer_Adam);
               begin
                  Put_Line (Routine_Name & "Adam");
                  Stochastic_Optimizers.C_Init
                    (Optimizer.Adam, Params => Parameters,
                     Initial_Learning_Rate => Self.Parameters.Learning_Rate_Init,
                     Beta_1 => Self.Parameters.Beta_1,
                     Beta_2 => Self.Parameters.Beta_2,
                     Epsilon => Self.Parameters.Epsilon);

                  Self.Attributes.Optimizer := Optimizer;
               end;

            when Lbfgs_Solver =>
               Put_Line (Routine_Name & "Lbfgs");
               declare
                  Optimizer : Optimizer_Record (Optimizer_SGD);
               begin
                  Stochastic_Optimizers.C_Init
                    (Optimizer.SGD, Params => Parameters,
                     Initial_Learning_Rate =>
                       Self.Parameters.Learning_Rate_Init,
                     Learning_Rate => Self.Parameters.Learning_Rate,
                     Momentum => Self.Parameters.Momentum,
                     Use_Nesterov => Self.Parameters.Nesterovs_Momentum,
                     Power_T => Self.Parameters.Power_T);

                  Self.Attributes.Optimizer := Optimizer;
               end;

            when Sgd_Solver =>
               Put_Line (Routine_Name & "SGD");
               declare
                  Optimizer : Optimizer_Record (Optimizer_SGD);
               begin
                  Stochastic_Optimizers.C_Init
                    (Optimizer.SGD, Params => Parameters,
                     Initial_Learning_Rate => Self.Parameters.Learning_Rate_Init,
                     Learning_Rate => Self.Parameters.Learning_Rate,
                     Momentum => Self.Parameters.Momentum,
                     Use_Nesterov => Self.Parameters.Nesterovs_Momentum,
                     Power_T => Self.Parameters.Power_T);

                  Self.Attributes.Optimizer := Optimizer;
               end;
         end case;
      end if;

      --  L597
      if Early_Stopping then
         Should_Stratify := Is_Classifier;
         if Should_Stratify then
            Stratify := Y;
         end if;

         Data_Splitter2.Train_Test_Split
           (X => X, Y => Y,
            Train_Size => Train_Size, Test_Size  => Test_Size,
            Train_X => Train_X, Train_Y => Train_Y,
            Test_X  => Test_X, Test_Y => Test_Y);
         if Is_Classifier then
            Test_Y := Label.Inverse_Transform (LE_U, Test_Y);
         end if;
      end if;

      --  L617
      Batch_Size := Self.Parameters.Batch_Size;
      if Batch_Size = 0 then
         Batch_Size := Integer'Min (200, Num_Samples);
      elsif Batch_Size > Num_Samples then
         Put_Line (Routine_Name & "WARNING: Batch size" &
                     Integer'Image (Self.Parameters.Batch_Size)  &
                     " clipped to" & Integer'Image (Num_Samples));
         Batch_Size := Num_Samples;
      end if;

      --        Max_Sample_Index := Num_Samples;
      --  L628
      --        Put_Line (Routine_Name & "Num_Samples: " & Integer'Image(Batch_Size));
      --        Put_Line (Routine_Name & "Batch_Size: " & Integer'Image(Batch_Size));
      for iter in 1 .. Self.Parameters.Max_Iter loop
         if Self.Parameters.Shuffle then
            null;
         end if;

         Accumulated_Loss := 0.0;
         --  Batches is a list of slice lists
         Batches := Utils.Gen_Batches (Num_Samples, Batch_Size);
         --           Put_Line (Routine_Name & "iter:" &  Integer'Image(iter) & "  Batches size: " &
         --                       Integer'Image (Integer (Batches.Length)));
         --           Sample_Index := 1;
         --        while Sample_Index <= Max_Sample_Index loop
         --  if Self.Parameters.Shuffle then
         --      Sample_Index := Shuffle (Sample_Index, Random_State);
         --  end if;
         --           Printing.Print_Slices (Routine_Name & " Batches", Batches);
         --  L636
         for batch_index in Batches.First_Index .. Batches.Last_Index loop
            --              Put_Line (Routine_Name & "L636 Batch index:" &
            --                          Integer'Image (batch_index));
            Batch_Slice := Batches (batch_index);
            Printing.Print_Slice (Routine_Name & "Slice", Batch_Slice);
            X_Batch.Clear;
            Y_Batch.Clear;

            for index in Batch_Slice.First .. Batch_Slice.Last loop
               X_Batch.Append (X (index));
               Y_Batch.Append (Y (index));
            end loop;
            --              Put_Line (Routine_Name & "X_Batch size:" &
            --                          Integer'Image (Integer (X_Batch.Length)));

            Activations.Replace_Element (1, X_Batch);

            --  L645
            Backprop (Self, X_Batch, Y_Batch, Activations, Deltas, Batch_Loss,
                      Coef_Grads, Intercept_Grads);
            Accumulated_Loss := Accumulated_Loss + Batch_Loss *
              Float (Batch_Slice.Last - Batch_Slice.First + 1);
            Put_Line (Routine_Name & "Accumulated_Loss: " &
                        Float'Image (Accumulated_Loss));
            --  L657 update weights
            Parameters.Coeff_Params := Coeff_Params;
            Parameters.Intercept_Params := Intercept_Params;
            Grads.Coeff_Params := Coef_Grads;
            Grads.Intercept_Params := Intercept_Grads;
            Put_Line (Routine_Name & "Intercept_Params set");
            Stochastic_Optimizers.Update_Params
              (Self.Attributes.Optimizer, Parameters, Grads);
            Put_Line (Routine_Name & "Update_Params set");

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
   --  L119  Forward_Pass performs a forward pass on the neural network by
   --  computing the values of the neurons in the hidden layers and the
   --  output layer.
   procedure Forward_Pass (Self        : in out MLP_Classifier;
                           Activations : in out Float_List_3D) is
      --  The ith element of Activations (length n_layers - 1) holds the values
      --  of the ith layer.
      --  Activations: layers x samples x features
      use Ada.Containers;
      use Base_Neural;
      use Float_Package;
      use Float_List_Package;
      Routine_Name       : constant String :=
                             "Multilayer_Perceptron.Forward_Pass ";
      Hidden_Activation  : constant Activation_Type :=
                             Self.Parameters.Activation;
      Output_Activation  : constant Activation_Type :=
                             Self.Attributes.Out_Activation;
      Num_Layers         : constant Positive := Self.Attributes.N_Layers;
      Coefficient_Matrix : Float_List_2D;  --  layer x X
      Acts_Dot_Coeffs    : Float_List_2D;
      Act_With_Intercept : Float_List_2D;
   begin
      --  Iterate over the hidden layers
      --  The Python range(stop) function returns a sequence of numbers,
      --   starting from 0 by default, incrementing by 1 (by default) and
      --   stopping BEFORE "stop". That is, at "stop" - 1.
      --  Therefore range(self.n_layers_ - 1): range is
      --            first (0) .. last (n_layers_ - 2)
      for index in 1 .. Num_Layers - 1 loop
--           Put_Line (Routine_Name & "index:" & Integer'Image (index));
         Coefficient_Matrix := Self.Attributes.Neuron_Coef_Layers (index);
         --  Dot (samples x features, samples x coefficients (weights))
         --  => samples x (coefficient * feature)
         Acts_Dot_Coeffs := Dot (Activations (index), Coefficient_Matrix);
         if Integer (Activations.Length) <= index then
            Activations.Set_Length (Count_Type (index + 1));
         end if;
         Activations (index + 1) := Acts_Dot_Coeffs;
--           Put_Line (Routine_Name & "Activations length:" &
--                      Count_Type'Image (Activations.Length));
--           Put_Line (Routine_Name & "Activations length (index + 1):" &
--                      Count_Type'Image (Activations (index + 1).Length));
         Act_With_Intercept := Activations (index + 1);
         --  Intercepts: layers x intercept values
         for intercept in Self.Attributes.Intercepts.First_Index ..
           Self.Attributes.Intercepts.Last_Index loop
--              Put_Line (Routine_Name & "intercept:" & Integer'Image (intercept));
            Act_With_Intercept.Replace_Element
              (intercept, Act_With_Intercept (intercept) +
               Self.Attributes.Intercepts (intercept));
         end loop;
--           Put_Line (Routine_Name & "Activations length:" &
--                      Count_Type'Image (Activations.Length));
         Activations.Replace_Element (index + 1, Act_With_Intercept);

         --  L134 For the hidden layers
         --           if index + 1 /= Num_Layers - 1 then
         if index + 1 /= Num_Layers then
            case Hidden_Activation is
               when Identity_Activation => null;
               when Logistic_Activation => Logistic (Activations (index + 1));
               when Tanh_Activation => Tanh (Activations (index + 1));
               when Relu_Activation => Relu (Activations (index + 1));
               when Softmax_Activation => Softmax (Activations (index + 1));
            end case;
         end if;
      end loop;

      --  L138 For the last layer
      case Output_Activation is
         when Identity_Activation => null;
         when Logistic_Activation => Logistic (Activations (Num_Layers));
         when Tanh_Activation => Tanh (Activations (Num_Layers));
         when Relu_Activation => Relu (Activations (Num_Layers));
         when Softmax_Activation => Softmax (Activations (Num_Layers));
      end case;

      Put_Line (Routine_Name & "Activations length:" &
                    Count_Type'Image (Activations.Length));
      Put_Line (Routine_Name & "Activations length (1):" &
                    Count_Type'Image (Activations (1).Length));
      Put_Line (Routine_Name & "Activations length (2):" &
                    Count_Type'Image (Activations (2).Length));
   end Forward_Pass;

   --  -------------------------------------------------------------------------

   --  L360  BaseMultilayerPerceptron._init_coef
   procedure Init_Coeff (Self            : in out MLP_Classifier;
                         Fan_In, Fan_Out : Positive;
                         Coef_Init       : out Float_List_2D;
                         Intercept_Init  : out Float_List) is
      use Maths;
      use Float_Math_Functions;
      use Base_Neural;
      Factor         : Float;
      Init_Bound     : Float;
      Coef_Init_1    : Float_List;
   begin
      if Self.Parameters.Activation = Logistic_Activation then
         Factor := 2.0;
      else
         Factor := 6.0;
      end if;

      Init_Bound := Sqrt (Factor / Float (Fan_In + Fan_Out));
      --  Generate weights and bias
      Coef_Init.Clear;
      Intercept_Init.Clear;
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

   --  L320
   procedure Initialize (Self        : in out MLP_Classifier;
                         Layer_Units : Integer_List) is
      use Ada.Containers;
      use Base_Neural;
      Routine_Name   : constant String := "Multilayer_Perceptron.Initialize ";
      Coef_Init      : Float_List_2D;
      Intercept_Init : Float_List;
   begin
      Self.Attributes.N_Iter := 0;
      Self.Attributes.T := 0;
      Self.Attributes.N_Layers := Natural (Layer_Units.Length);
      Self.Attributes.Out_Activation := Logistic_Activation;
      Self.Attributes.Neuron_Coef_Layers.Clear;
      Self.Attributes.Intercepts.Clear;

      --  L344
      Put_Line (Routine_Name & "N_Layers" &
                  Integer'Image (Self.Attributes.N_Layers));
      for layer in 1 .. Self.Attributes.N_Layers - 1 loop
         Put_Line (Routine_Name & "layer" & Integer'Image (layer));
         Init_Coeff (Self, Layer_Units.Element (layer),
                     Layer_Units.Element (layer + 1),
                     Coef_Init, Intercept_Init);
         --  Add coefficent matrices and intercept vectors for layer.
         Self.Attributes.Neuron_Coef_Layers.Append (Coef_Init);
         Self.Attributes.Intercepts.Append (Intercept_Init);
      end loop;
      Put_Line (Routine_Name & "Neuron_Coef_Layers size" &
                  Count_Type'Image (Self.Attributes.Neuron_Coef_Layers.Length) &
                  Count_Type'Image (Self.Attributes.Neuron_Coef_Layers (1).Length) &
                  Count_Type'Image (Self.Attributes.Neuron_Coef_Layers (1) (1).Length));
      Put_Line (Routine_Name & "Intercepts size" &
                  Count_Type'Image (Self.Attributes.Intercepts.Length) &
                  Count_Type'Image (Self.Attributes.Intercepts (1).Length));
      --        Printing.Print_Float_Lists_3D ("Neuron_Coef_Layers", Self.Attributes.Neuron_Coef_Layers);
      --        Printing.Print_Float_Lists_2D ("Intercepts", Self.Attributes.Intercepts);

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
     (Self  : in out MLP_Classifier; Early_Stopping : Boolean;
      X_Val : Float_List_2D) is
      Score_Val : Float;
   begin
      if Early_Stopping then
         Score_Val := Base_Mix.Score (X_Val);
         Self.Parameters.Validation_Scores.Append (Score_Val);
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
                             --                               X                  : Float_List_2D;
                             Y                  : Integer_List) is
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
