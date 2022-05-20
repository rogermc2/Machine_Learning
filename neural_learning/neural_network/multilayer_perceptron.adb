--  Based on scikit-learn/sklearn/neural_network/_multilayer_perceptron.py

--  The simplest representation of a neural network is a Multilayer Perceptron.
--  In its simplest form a MLP is just three layers.
--  An input layer represented by a real matrix X (N×d) where N is the number
--  of training examples and d is the number of features.
--  A hidden layer which is usually a Rect_LU or a logistic sigmoid function.
--  Hidden layer i could be a Rect_LU function represented by
--  h_i(x) = ReLU (x) = max (x, 0)
--  In other words, if the input to the Rect_LU function is negative, the function
--  outputs a 0.
--  If the inputs x are positive, the Rect_LU function will output x.
--  The hidden layer feeds into the output layer which is just another function.
--  This function could be squared error function (in the context of regression)
--  or softmax (in the case of multiclass classification).
--  The MLP is complete when you consider the weight and bias matrices but
--  we don't need them for now.
--  The activation function is just what the name suggests; a function.
--  In the example above, the activation function for the hidden layer is the
--  Rect_LU function.
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
--  with Ada.Calendar; use Ada.Calendar;
with Ada.Containers;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;

with Maths;

with Base;
with Data_Splitter;
with Multiclass_Utils;
with Neural_Maths;
with Printing;
with Utils;

package body Multilayer_Perceptron is

   First_Pass : Boolean := True;

   procedure Compute_Loss_Gradient
     (Self          : MLP_Classifier;
      Layer         : Positive;
      Num_Samples   : Positive;
      Activations   : Real_Matrix_List;
      Deltas        : Real_Matrix_List;
      Grads         : in out Parameters_List);
   --     procedure Fit_Lbfgs (Self        : in out MLP_Classifier;
   --                          X           : Float_List_2D;
   --                          Y           : Integer_List_2D;
   --                          Activations  : Float_List_3D;
   --                          Deltas       : Float_List_3D;
   --                          Grads       : in out Parameters_List;
   --                          Layer_Units : Integer_List);
   procedure Fit_Stochastic (Self         : in out MLP_Classifier;
                             X            : Real_Float_Matrix;
                             Y            : Boolean_Matrix;
                             Incremental  : Boolean := False);
   procedure Forward_Pass (Self         : MLP_Classifier;
                           Activations  : in out Real_Matrix_List);
   function Forward_Pass_Fast (Self  : MLP_Classifier; X : Real_Float_Matrix)
                                return Real_Float_Matrix;
   procedure Initialize (Self        : in out MLP_Classifier;
                         Layer_Units : NL_Types.Integer_List);
   function Init_Coeff (Self            : in out MLP_Classifier;
                        Fan_In, Fan_Out : Positive) return Parameters_Record;
   procedure Init_Optimizer (Self : in out MLP_Classifier);
   procedure Process_Batch (Self             : in out MLP_Classifier;
                            X                : Real_Float_Matrix;
                            Y                : Boolean_Matrix;
                            Batch_Slice      : NL_Types.Slice_Record;
                            Batch_Size       : Positive;
                            Accumulated_Loss : in out Float);
   procedure Update_No_Improvement_Count
     (Self         : in out MLP_Classifier; Early_Stopping : Boolean;
      X_Val, Y_Val : Real_Float_Matrix);
   procedure Update_Grads (Self               : in out MLP_Classifier;
                           Activations        : Real_Matrix_List;
                           Deltas             : in out Real_Matrix_List;
                           Grads              : in out Parameters_List;
                           Layer, Num_Samples : Positive);
   procedure Validate_Hyperparameters (Self : MLP_Classifier);
   function Validate_Input (Self        : in out MLP_Classifier;
                            Y           : Integer_Matrix;
                            Incremental : Boolean) return Boolean_Matrix;

   --  -------------------------------------------------------------------------
   --  L241  Backprop computes the MLP loss function and its derivatives
   --       with respect to each parameter: weights and bias vectors.
   procedure Backprop (Self        : in out MLP_Classifier;
                       X           : Real_Float_Matrix;
                       Y           : Boolean_Matrix;
                       Activations : in out Real_Matrix_List;
                       Loss        : out Float;
                       Grads       : out Parameters_List) is
      use Ada.Containers;
      use Base_Neural;
      use Parameters_Package;
      use Real_Float_Arrays;
      use Real_Matrix_List_Package;
      --        Routine_Name       : constant String := "Multilayer_Perceptron.Backprop ";
      Num_Samples        : constant Positive := Positive (X'Length);
      Last               : constant Positive := Self.Attributes.N_Layers - 1;
      Loss_Function_Name : Loss_Function;
      Deltas             : Real_Matrix_List;
      Sum_Sq_Coeffs      : Float;
   begin
      --  L284
      if Self.Attributes.Loss_Function_Name = Log_Loss_Function and then
        Self.Attributes.Out_Activation = Logistic_Activation then
         Loss_Function_Name := Binary_Log_Loss_Function;
      else
         Loss_Function_Name := Self.Attributes.Loss_Function_Name;
      end if;

      case Loss_Function_Name is
         when Binary_Log_Loss_Function =>
            Loss := Binary_Log_Loss (Y, Activations.Last_Element);
         when Log_Loss_Function =>
            Loss := Log_Loss (Y, Activations.Last_Element);
         when Squared_Error_Function => null;
            Loss := Squared_Loss (Y, Activations.Last_Element);
      end case;

      --  L289  Add L2 regularization term to loss
      --  for s in self.coefs_:
      Sum_Sq_Coeffs := 0.0;
      for layer in Self.Attributes.Params.First_Index ..
        Self.Attributes.Params.Last_Index loop
         declare
            Coeffs : constant Real_Float_Matrix :=
                       Self.Attributes.Params (Layer).Coeff_Grads;
            --  numpy.ravel (a) returns the elements of a as a 1-D array.
            Ravel  : Real_Float_Vector (1 .. Coeffs'Length * Coeffs'Length (2));
         begin
            for row in Coeffs'Range loop
               for col in Coeffs'Range (2) loop
                  Ravel ((row - 1) * Coeffs'Length (2) +
                           col - Coeffs'First (2) + 1) := Coeffs (row, col);
               end loop;
            end loop;
            Sum_Sq_Coeffs := Sum_Sq_Coeffs + Ravel * Ravel;
         end;  --  declare
      end loop;

      --  L292
      Loss := Loss + 0.5 * (Self.Parameters.Alpha *
                              Sum_Sq_Coeffs / Float (Num_Samples));

      --  L297 Backward propagate
      --  The calculation of delta[last]  works with the following combinations
      --  of output activation and loss function:
      --  sigmoid and binary cross entropy, softmax and categorical cross
      --  entropy and identity with squared loss.
      --  The ith element of deltas holds the difference between the activations
      --  of the i + 1 layer and the backpropagated error.
      --  Deltas are gradients of loss with respect to z in each layer.
      --  The ith element of Activations (batch samples x values) is a list of
      --  the ith layer values of a batch.

      --  L301  Initialize Deltas
      Deltas.Set_Length (Count_Type (Self.Attributes.N_Layers - 1));
      Deltas.Replace_Element (Deltas.Last_Index, Activations.Last_Element - Y);

      --  L304  Compute gradient for the last layer
      Compute_Loss_Gradient
        (Self, Last, Num_Samples, Activations, Deltas, Grads);

      --  L310, L308
      for layer in reverse 2 .. Self.Attributes.N_Layers - 1 loop
         Update_Grads (Self, Activations, Deltas, Grads, layer,
                       Num_Samples);
      end loop;

   end Backprop;

   --  -------------------------------------------------------------------------

   procedure Check_Weights (Self : MLP_Classifier) is
      use Parameters_Package;
      Routine_Name : constant String :=
                       "Multilayer_Perceptron.Check_Weights ";
      Weight       : Float;
      Bad          : Boolean := False;
   begin
      for Param_Cursor of Self.Attributes.Params loop
         declare
            Params : constant Parameters_Record := Param_Cursor;
         begin
            for row in Params.Coeff_Grads'Range loop
               for col in Params.Coeff_Grads'Range (2) loop
                  Weight := Params.Coeff_Grads (row, col);
                  Bad := Bad and not Weight'Valid;
               end loop;
            end loop;
            for row in Params.Intercept_Grads'Range loop
               Weight := Params.Intercept_Grads (row);
               Bad := Bad and not Weight'Valid;
            end loop;
         end;
      end loop;

      Assert (not Bad, Routine_Name &
                " the solver produced non-finite parameter weights." &
                " The input data may contain large values and needs to be" &
                " preprocessed.");

   end Check_Weights;

   --  -------------------------------------------------------------------------
   --  L1054
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
      Classifier.Attributes.Loss_Function_Name  := Loss_Function_Name;
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

   --  Coef_Grads is a 3D list of weight matrices where the weight matrix at
   --  index i represents the weights between layer i and layer i + 1.
   --  Intercept_Grads is a 2D list of bias vectors where the vector at index
   --  the bias values added to layer i + 1.
   procedure Compute_Loss_Gradient
     (Self          : MLP_Classifier;
      Layer         : Positive;
      Num_Samples   : Positive;
      Activations   : Real_Matrix_List;
      Deltas        : Real_Matrix_List;
      Grads         : in out Parameters_List) is
      use Real_Float_Arrays;
      --        Routine_Name        : constant String :=
      --                                "Multilayer_Perceptron.Compute_Loss_Gradient ";
      --  L185
      Act_Delta           : constant Real_Float_Matrix
        := Transpose (Activations (Layer)) * Deltas (Layer);
      --  Mean computes mean of values along the specified axis.
      Delta_Mean          : constant Real_Float_Vector :=
                              Neural_Maths.Mean (Deltas (Layer));
   begin
      --  L185
      --  The ith element of Deltas holds the difference between the
      --  activations of the i + 1 layer and the backpropagated error.
      --  An activation converts the output from a layer into a form that is
      --  suitable for input into the next layer

      declare
         --  Coeff_Grads is a list of fan_in x fan_out matrices
         --  L186
         New_Coeff_Grads     : Real_Float_Matrix := Act_Delta;
         New_Intercept_Grads : constant Real_Float_Vector := Delta_Mean;
         New_Grad            : Parameters_Record
           (New_Coeff_Grads'Length, New_Coeff_Grads'Length (2));
      begin
         New_Coeff_Grads := (New_Coeff_Grads + Self.Parameters.Alpha *
                               Self.Attributes.Params (layer).Coeff_Grads) / Float (Num_Samples);
         New_Grad.Coeff_Grads := New_Coeff_Grads;
         New_Grad.Intercept_Grads := New_Intercept_Grads;
         Grads.Prepend (New_Grad);
      end;

   end  Compute_Loss_Gradient;

   --  -------------------------------------------------------------------------
   --  L377  MultilayerPerceptron._Fit
   procedure Fit (Self        : in out MLP_Classifier;
                  X           : Real_Float_Matrix;
                  Y           : Integer_Matrix;
                  Incremental : Boolean := False) is
      use Ada.Containers;
      --          Routine_Name       : constant String :=
      --                                 "Multilayer_Perceptron.Fit ";
      Num_Features       : constant Positive := Positive (X'Length (2));
      Hidden_Layer_Sizes : constant NL_Types.Integer_List :=
                             Self.Parameters.Hidden_Layer_Sizes;
      Y_Bin              : constant Boolean_Matrix :=
                             Validate_Input (Self, Y, Incremental);
      Layer_Units        : NL_Types.Integer_List;
   begin
      Validate_Hyperparameters (Self);
      First_Pass :=
        Self.Attributes.Params.Is_Empty or else
        (not Self.Parameters.Warm_Start and then not Incremental);

      Self.Attributes.N_Outputs := Positive (Y_Bin'Length (2));
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

      --  L417 Initialized grads are empty vectors, no initialization required.
      --  L427
      if Self.Parameters.Solver = Sgd_Solver or else
        Self.Parameters.Solver = Adam_Solver then
         Fit_Stochastic (Self, X, Y_Bin, Incremental);

      elsif Self.Parameters.Solver = Lbfgs_Solver then
         null;
         --           Fit_Lbfgs (Self, X, Y_2D, Activations, Deltas, Grads, Layer_Units);
      end if;

      Check_Weights (Self);

   end Fit;

   --  -------------------------------------------------------------------------
   --  L516
   --     procedure Fit_Lbfgs (Self            : in out MLP_Classifier;
   --                          X               : Float_List_2D;
   --                          Y               : Integer_List_2D;
   --                          Activations     : in out Float_List_3D;
   --                          Deltas          : in out Float_List_3D;
   --                          Grads           : in out Parameters_List;
   --                          Layer_Units     : Integer_List) is
   --        Routine_Name : constant String := "Multilayer_Perceptron.Fit_Lbfgs ";
   --        Num_Samples  : constant Positive := Positive (X.Length);
   --        Start        : Positive := 1;
   --        Last         : Positive;
   --        N_Fan_In     : Positive;
   --        N_Fan_Out    : Positive;
   --     begin
   --        Self.Attributes.Coef_Indptr.Clear;
   --        Self.Attributes.Intercept_Indptr.Clear;

   --  L524  Save sizes and indices of coefficients for faster unpacking
   --        for index in 1 .. Self.Attributes.N_Layers - 1 loop
   --           N_Fan_In := Layer_Units.Element (index);
   --           N_Fan_Out := Layer_Units.Element (index + 1);
   --           Last := Start + N_Fan_In * N_Fan_Out;
   --           Self.Attributes.Coef_Indptr.Append ((Start, Last,
   --                                               N_Fan_In, N_Fan_Out));
   --           Start := Last + 1;
   --        end loop;

   --  L532  Save sizes and indices of intercepts for faster unpacking
   --        Start := 1;
   --        for index in 1 .. Self.Attributes.N_Layers - 1 loop
   --           Last := Start + N_Fan_In * N_Fan_Out;
   --           Self.Attributes.Intercept_Indptr.Append ((Start, Last));
   --           Start := Last + 1;
   --        end loop;
   --
   --        Assert (False, Routine_Name & "coding incomplete.");
   --
   --     end Fit_Lbfgs;

   --  -------------------------------------------------------------------------

   --  L563
   procedure Fit_Stochastic (Self        : in out MLP_Classifier;
                             X           : Real_Float_Matrix;
                             Y           : Boolean_Matrix;
                             Incremental : Boolean := False) is
      use Estimator;
      Routine_Name                       : constant String :=
                                             "Multilayer_Perceptron.Fit_Stochastic ";
      Is_Classifier                      : constant Boolean :=
                                             Self.Estimator_Kind = Classifier_Estimator;
      Num_Samples                        : constant Positive := Positive (X'Length);
      Num_Features                       : constant Positive := Positive (X'Length (2));
      Num_Classes                        : constant Positive :=
                                             Positive (Self.Attributes.Classes.Length);
      LE_U                               : Label.Label_Binarizer;
      Iter                               : Natural := 0;
      Continue                           : Boolean := True;
      Early_Stopping                     : constant Boolean
        := Self.Parameters.Early_Stopping and
      then not Incremental;
      Test_Size                          : constant Positive
        := Positive (Self.Parameters.Validation_Fraction *
                       Float (Num_Samples));
      Train_Size                         : constant Positive := Num_Samples - Test_Size;
      --          Stratify               : Boolean_Matrix (Y'Range, Y'Range (2));
      --          Should_Stratify        : Boolean;
      Train_X                            : Real_Float_Matrix
        (1 .. Train_Size, 1 .. Num_Features);
      Train_Y                            : Boolean_Matrix
        (1 .. Train_Size, 1 .. Num_Classes);
      Test_X                             : Real_Float_Matrix
        (1 .. Test_Size, 1 .. Num_Features);
      Test_Y                             : NL_Arrays_And_Matrices.Boolean_Matrix
        (1 .. Test_Size, 1 .. Num_Classes);
      Val_Y                              : Real_Float_Matrix
        (1 .. Test_Size, 1 .. Num_Classes) := (others => (others => 0.0));
      Batch_Size                         : Positive;
      Batches                            : NL_Types.Slices_List;
      Accumulated_Loss                   : Float := 0.0;
      Msg                                : Unbounded_String;
      Is_Stopping                        : Boolean := False;
   begin
      --  L576
      if not Incremental or else
        Self.Attributes.Optimizer.Kind = No_Optimizer then
         Init_Optimizer (Self);
      end if;

      --  L597
      if Early_Stopping then
         Put_Line (Routine_Name & "L597  *** Early_Stopping ***");
         --              Should_Stratify := Is_Classifier and Self.Attributes.N_Outputs = 1;
         --              if Should_Stratify then
         --                  Stratify := Y;
         --              end if;
         Data_Splitter.Train_Test_Split
           (X => X, Y => Y,
            Train_Size => Train_Size, Test_Size  => Test_Size,
            Train_X => Train_X, Train_Y => Train_Y,
            Test_X  => Test_X, Test_Y => Test_Y);

         if Is_Classifier then
            Val_Y := Label.Inverse_Transform (LE_U, Test_Y);
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
      --  Batches is a list of slice lists
      Batches := Utils.Gen_Batches (Num_Samples, Batch_Size);

      --  L628
      while Continue and then Iter < Self.Parameters.Max_Iter loop
         Iter := Iter + 1;
         if Self.Parameters.Shuffle then
            null;
         end if;

         Accumulated_Loss := 0.0;
         --  L636
         for batch_index in Batches.First_Index ..
           Batches.Last_Index loop
            Process_Batch (Self, X, Y, Batches (Batch_Index), Batch_Size,
                           Accumulated_Loss);
         end loop;

         --  L661
         Self.Attributes.N_Iter := Self.Attributes.N_Iter + 1;
         Self.Attributes.Loss := Accumulated_Loss / Float (Num_Samples);
         Self.Attributes.T := Self.Attributes.T + Num_Samples;
         Self.Attributes.Loss_Curve.Append (Self.Attributes.Loss);
         --  L668
         if Self.Parameters.Verbose then
            Put_Line (Routine_Name & "L668 Iteration" &
                        Integer'Image (Self.Attributes.N_Iter) &
                        ", loss = " & Float'Image (Self.Attributes.Loss));
         end if;

         --  L669 Update no_improvement_count based on training loss or
         --       validation score according to early_stopping
         Update_No_Improvement_Count (Self, Early_Stopping, Test_X, Val_Y);
         --  for learning rate that needs to be updated at iteration end;
         if Self.Attributes.Optimizer.Kind = Optimizer_SGD then
            null;
         end if;

         --  676
         if Self.Attributes.No_Improvement_Count >
           Self.Parameters.N_Iter_No_Change then
            if Early_Stopping then
               Msg := Msg & To_Unbounded_String (Routine_Name & "Validation score");
            else
               Msg := Msg & To_Unbounded_String (Routine_Name & "Training loss");
            end if;

            Msg := Msg & To_Unbounded_String
              (" did not improve more than tolerance =" &
                 Float'Image (Self.Parameters.Tol) & " for" &
                 Integer'Image (Self.Parameters.N_Iter_No_Change) &
                 " consecutive epochs.");
            --  L692
            Is_Stopping :=
              Trigger_Stopping (Self.Attributes.Optimizer, To_String (Msg),
                                Self.Parameters.Verbose);
            Continue := not Is_Stopping;
            if Continue then
               Self.Attributes.No_Improvement_Count := 0;
            end if;
         end if;

         Continue := Continue and not Incremental;
         if Continue and
           Self.Attributes.N_Iter = Self.Parameters.Max_Iter then
            Put_Line ("    Maximum iterations (" &
                        Integer'Image (Self.Parameters.Max_Iter) &
                        ") reached but the optimization hasn't " &
                        "converged yet.");
            New_Line;
         end if;
      end loop;

      --  L711
      if Early_Stopping then
         Self.Attributes.Params := Self.Parameters.Best_Params;
      end if;

      if Self.Parameters.Verbose then
         Put_Line (Routine_Name & "Number of iterations: " & Integer'Image (Iter) &
                     "  loss: " & Float'Image (Self.Attributes.Loss));
      end if;

   end Fit_Stochastic;

   --  -------------------------------------------------------------------------
   --  L119  Forward_Pass performs a forward pass on the neural network by
   --  computing the activation values of the neurons in the hidden layers and
   --   the output layer.
   procedure Forward_Pass (Self        : MLP_Classifier;
                           Activations : in out Real_Matrix_List) is
      --  The ith element of Activations (length n_layers - 1) holds
      --  the activation values of the ith layer.
      use Base_Neural;
      use Parameters_Package;
      --        Routine_Name      : constant String :=
      --                              "Multilayer_Perceptron.Forward_Pass ";
      Hidden_Activation : constant Activation_Type :=
                            Self.Parameters.Activation;
      Output_Activation : constant Activation_Type :=
                            Self.Attributes.Out_Activation;
      Num_Layers        : constant Positive := Self.Attributes.N_Layers;
   begin
      --  L130
      for layer in 1 .. Num_Layers - 1 loop
         declare
            use Real_Float_Arrays;
            Params             : constant Parameters_Record :=
                                   Self.Attributes.Params (layer);
            Activat_Dot_Coeff  : constant Real_Float_Matrix :=
                                   Activations (layer) * Params.Coeff_Grads;
         begin
            --  L131 Add layer + 1
            Activations.Append (Activat_Dot_Coeff + Params.Intercept_Grads);

            --  L134 For the hidden layers
            if layer /= Num_Layers - 1 then
               case Hidden_Activation is
                  when Identity_Activation => null;
                  when Logistic_Activation =>
                     Logistic (Activations (layer + 1));
                  when Tanh_Activation => Tanh (Activations (layer + 1));
                  when Rect_LU_Activation => Rect_LU (Activations (layer + 1));
                  when Softmax_Activation => Softmax (Activations (layer + 1));
               end case;
            end if;
         end;  --  declare
      end loop;

      --  L138 For the last layer
      case Output_Activation is
         when Identity_Activation => null;
         when Logistic_Activation =>
            Logistic (Activations (Activations.Last_Index));
         when Tanh_Activation => Tanh (Activations (Activations.Last_Index));
         when Rect_LU_Activation => Rect_LU (Activations (Activations.Last_Index));
         when Softmax_Activation =>
            Softmax (Activations (Activations.Last_Index));
      end case;

   end Forward_Pass;

   --  -------------------------------------------------------------------------

   function Forward_Pass_Fast (Self  : MLP_Classifier; X : Real_Float_Matrix)
                                return Real_Float_Matrix is
      --        use Ada.Containers;
      use Base_Neural;
      use Real_Float_Arrays;
      use Parameters_Package;
      --        Routine_Name       : constant String :=
      --                               "Multilayer_Perceptron.Forward_Pass_Fast ";
      Hidden_Activation  : constant Activation_Type :=
                             Self.Parameters.Activation;
      Output_Activation  : constant Activation_Type :=
                             Self.Attributes.Out_Activation;
      Num_Layers         : constant Positive := Self.Attributes.N_Layers;
      Params_List        : constant Parameters_List := Self.Attributes.Params;
      Activ_Out          : Real_Float_Matrix (X'Range,
                                              1 .. Self.Attributes.N_Outputs);
      --  One element list used to allow for different sized matrices
      Prev_Activation    : Real_Matrix_List;
   begin
      --  Initialize first layer
      Prev_Activation.Append (X);

      --  L167 Forward propagate
      for layer in 1 .. Num_Layers - 1 loop
         declare
            Params             : constant Parameters_Record :=
                                   Params_List (layer);
            Updated_Activation : Real_Float_Matrix :=
                                   Prev_Activation (1) * Params.Coeff_Grads;
         begin
            Updated_Activation :=
              Updated_Activation + Params.Intercept_Grads;

            if layer /= Num_Layers - 1 then
               case Hidden_Activation is
                  when Identity_Activation => null;
                  when Logistic_Activation => Logistic (Updated_Activation);
                  when Tanh_Activation => Tanh (Updated_Activation);
                  when Rect_LU_Activation => Rect_LU (Updated_Activation);
                  when Softmax_Activation => Softmax (Updated_Activation);
               end case;
            end if;
            Prev_Activation.Replace_Element (1, Updated_Activation);

            if layer = Num_Layers - 1 then
               Activ_Out := Updated_Activation;
            end if;
         end;
      end loop;

      --  L172
      case Output_Activation is
         when Identity_Activation => null;
         when Logistic_Activation => Logistic (Activ_Out);
         when Tanh_Activation => Tanh (Activ_Out);
         when Rect_LU_Activation => Rect_LU (Activ_Out);
         when Softmax_Activation => Softmax (Activ_Out);
      end case;

      return Activ_Out;

   end Forward_Pass_Fast;

   --  -------------------------------------------------------------------------

   --  L360  MultilayerPerceptron._init_coef
   function Init_Coeff (Self            : in out MLP_Classifier;
                        Fan_In, Fan_Out : Positive) return Parameters_Record is
      use Maths;
      use Float_Math_Functions;
      use Base_Neural;
      --        Routine_Name : constant String := "Multilayer_Perceptron.Init_Coeff ";
      Params       : Parameters_Record (Fan_In, Fan_Out);
      Factor       : Float;
      Init_Bound   : Float;
   begin
      if Self.Parameters.Activation = Logistic_Activation then
         Factor := 2.0;
      else
         Factor := 6.0;
      end if;

      Init_Bound := Sqrt (Factor / Float (Fan_In + Fan_Out));
      --  Generate random weights
      for f_in in 1 .. Fan_In loop
         for f_out in 1 .. Fan_Out loop
            Params.Coeff_Grads (f_in, f_out) := Init_Bound * Random_Float;
         end loop;
      end loop;

      --  Generate random bias
      for f_out in 1 .. Fan_Out loop
         Params.Intercept_Grads (f_out) := Init_Bound * Random_Float;
      end loop;

      return Params;

   end Init_Coeff;

   --  ------------------------------------------------------------------------
   --  L320
   procedure Initialize (Self        : in out MLP_Classifier;
                         Layer_Units : NL_Types.Integer_List) is
      use Base_Neural;
      use Estimator;
      use Multiclass_Utils;
      --        Routine_Name   : constant String := "Multilayer_Perceptron.Initialize ";
      Fan_In         : Positive;
      Fan_Out        : Positive;
   begin
      Self.Attributes.Loss := 0.0;
      Self.Attributes.Best_Loss := 0.0;
      Self.Attributes.Loss_Curve.Clear;
      Self.Attributes.No_Improvement_Count := 0;
      Self.Attributes.T := 0;
      Self.Attributes.Params.Clear;  --  Layers
      Self.Attributes.Coef_Indptr.Clear;
      Self.Attributes.Intercept_Indptr.Clear;
      Self.Attributes.T := 0;
      Self.Attributes.N_Layers := Natural (Layer_Units.Length);

      if Self.Estimator_Kind /= Classifier_Estimator then
         Self.Attributes.Out_Activation := Identity_Activation;
      elsif Self.Attributes.Binarizer.Y_Kind = Y_Multiclass
      then
         Self.Attributes.Out_Activation := Softmax_Activation;
      else
         Self.Attributes.Out_Activation := Logistic_Activation;
      end if;

      --  L344
      Self.Attributes.Params.Clear;
      for layer in 1 .. Self.Attributes.N_Layers - 1 loop
         --  Add coefficent matrices and intercept vectors for layer.
         Fan_In := Layer_Units (layer);
         Fan_Out := Layer_Units (layer + 1);
         Self.Attributes.Params.Append (Init_Coeff (Self, Fan_In, Fan_Out));
      end loop;

      --  L351
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

   procedure Init_Optimizer (Self : in out MLP_Classifier) is
   begin
      case Self.Parameters.Solver is
         when Adam_Solver =>
            declare
               Optimizer : Optimizer_Record (Optimizer_Adam);
            begin
               Stochastic_Optimizers.C_Init
                 (Optimizer.Adam, Params => Self.Attributes.Params,
                  Initial_Learning_Rate =>
                    Self.Parameters.Learning_Rate_Init,
                  Beta_1 => Self.Parameters.Beta_1,
                  Beta_2 => Self.Parameters.Beta_2,
                  Epsilon => Self.Parameters.Epsilon);

               Self.Attributes.Optimizer := Optimizer;
            end;

         when Lbfgs_Solver =>
            declare
               Optimizer : Optimizer_Record (Optimizer_SGD);
            begin
               Stochastic_Optimizers.C_Init
                 (Optimizer.SGD, Params => Self.Attributes.Params,
                  Initial_Learning_Rate =>
                    Self.Parameters.Learning_Rate_Init,
                  Learning_Rate => Self.Parameters.Learning_Rate,
                  Momentum => Self.Parameters.Momentum,
                  Use_Nesterov => Self.Parameters.Nesterovs_Momentum,
                  Power_T => Self.Parameters.Power_T);

               Self.Attributes.Optimizer := Optimizer;
            end;

         when Sgd_Solver =>
            declare
               Optimizer : Optimizer_Record (Optimizer_SGD);
            begin
               Stochastic_Optimizers.C_Init
                 (Optimizer.SGD, Params => Self.Attributes.Params,
                  Initial_Learning_Rate => Self.Parameters.Learning_Rate_Init,
                  Learning_Rate => Self.Parameters.Learning_Rate,
                  Momentum => Self.Parameters.Momentum,
                  Use_Nesterov => Self.Parameters.Nesterovs_Momentum,
                  Power_T => Self.Parameters.Power_T);

               Self.Attributes.Optimizer := Optimizer;
            end;
      end case;

   end Init_Optimizer;

   --  -------------------------------------------------------------------------

   function Predict (Self : MLP_Classifier; X : Real_Float_Matrix)
                      return Real_Float_Matrix is
      Routine_Name   : constant String := "Multilayer_Perceptron.Predict ";
      Y_Pred : constant Real_Float_Matrix := Forward_Pass_Fast (Self, X);
   begin
        Printing.Print_Float_Matrix (Routine_Name & "Y_Pred", Y_Pred, 1, 3);
      return Label.Inverse_Transform (Self.Attributes.Binarizer, Y_Pred);

   end Predict;

   --  -------------------------------------------------------------------------

   procedure Process_Batch (Self             : in out MLP_Classifier;
                            X                : Real_Float_Matrix;
                            Y                : Boolean_Matrix;
                            Batch_Slice      : NL_Types.Slice_Record;
                            Batch_Size       : Positive;
                            Accumulated_Loss : in out Float) is
      --        Routine_Name   : constant String :=
      --                           "Multilayer_Perceptron.Process_Batch ";
      Num_Features   : constant Positive := Positive (X'Length (2));
      Num_Classes    : constant Positive := Y'Length (2);
      X_Batch        : Real_Float_Matrix (1 .. Batch_Size, 1 .. Num_Features);
      Y_Batch        : Boolean_Matrix (1 .. Batch_Size, 1 .. Num_Classes);
      --  Activations: layers x samples x features
      Activations    : Real_Matrix_List;
      Grads          : Parameters_List;
      Batch_Row      : Positive;
      Batch_Loss     : Float;
   begin
      for row in Batch_Slice.First .. Batch_Slice.Last loop
         Batch_Row := row - Batch_Slice.First + 1;
         for col in 1 .. Num_Features loop
            X_Batch (Batch_Row, col) := X (row, col);
         end loop;
         for col in Y'Range (2) loop
            Y_Batch (Batch_Row, col) := Y (row, col);
         end loop;
      end loop;

      --  L645
      Activations.Clear;
      Activations.Append (X_Batch);
      Forward_Pass (Self, Activations);
      Backprop (Self, X_Batch, Y_Batch, Activations, Batch_Loss, Grads);

      --  L665
      Accumulated_Loss := Accumulated_Loss + Batch_Loss *
        Float (Batch_Slice.Last - Batch_Slice.First + 1);
      --  L667 update weights
      --  Update_Params updates parameters with given gradients
      Stochastic_Optimizers.Update_Params
        (Self.Attributes.Optimizer, Self.Attributes.Params, Grads);

   end Process_Batch;

   --  -------------------------------------------------------------------------

   procedure Update_Grads (Self               : in out MLP_Classifier;
                           Activations        : Real_Matrix_List;
                           Deltas             : in out Real_Matrix_List;
                           Grads              : in out Parameters_List;
                           Layer, Num_Samples : Positive) is
      use Base_Neural;
      use Real_Float_Arrays;
      use Real_Matrix_List_Package;
      --        Routine_Name : constant String := "Multilayer_Perceptron.Update_Grads ";
      Params       : constant Parameters_Record :=
                       Self.Attributes.Params (Layer);
   begin
      --  L311
      Deltas.Replace_Element (Layer - 1, Deltas.Element (Layer) *
                                Transpose (Params.Coeff_Grads));
      --  L312
      case Self.Parameters.Activation is
         when Identity_Activation => null;
         when Logistic_Activation =>
            Logistic_Derivative (Z => Activations (Layer),
                                 Del => Deltas (Layer - 1));
         when Tanh_Activation =>
            Tanh_Derivative (Activations (Layer), Deltas (Layer - 1));
         when Rect_LU_Activation =>
            Rect_LU_Derivative (Activations (Layer), Deltas (Layer - 1));
         when Softmax_Activation => null;
      end case;

      --  L314
      Compute_Loss_Gradient
        (Self => Self, Layer => Layer - 1, Num_Samples => Num_Samples,
         Activations => Activations, Deltas => Deltas, Grads => Grads);

   end Update_Grads;

   --  -------------------------------------------------------------------------
   --  L716
   procedure Update_No_Improvement_Count
     (Self          : in out MLP_Classifier; Early_Stopping : Boolean;
      X_Val, Y_Val  : Real_Float_Matrix) is
      Routine_Name     : constant String
        := "Multilayer_Perceptron.Update_No_Improvement_Count ";
      Sample_Weight    : constant Real_Float_Vector (1 .. 0) := (others => 0.0);
      Last_Valid_Score : Float;
      Score_Val        : Float ;
   begin
      if Early_Stopping then
         Score_Val := Base.Score (Self, X_Val, Y_Val, Sample_Weight);
         Self.Parameters.Validation_Scores.Append (Score_Val);
         Last_Valid_Score := Self.Parameters.Validation_Scores.Last_Element;
         if Self.Parameters.Verbose then
            Put_Line (Routine_Name & "Validation score: " &
                        Float'Image (Last_Valid_Score));
         end if;

         --  L728
         if Last_Valid_Score <
           Self.Parameters.Best_Validation_Score + Self.Parameters.Tol then
            Self.Attributes.No_Improvement_Count :=
              Self.Attributes.No_Improvement_Count + 1;
         else
            Self.Attributes.No_Improvement_Count := 0;
         end if;

         if Last_Valid_Score > Self.Parameters.Best_Validation_Score then
            Self.Parameters.Best_Validation_Score := Last_Valid_Score;
            Self.Parameters.Best_Params := Self.Attributes.Params;
         end if;

      else
         if Self.Attributes.Loss_Curve.Last_Element >
           Self.Attributes.Best_Loss then
            Self.Attributes.No_Improvement_Count :=
              Self.Attributes.No_Improvement_Count + 1;
         else
            Self.Attributes.No_Improvement_Count := 0;
         end if;

         if Self.Attributes.Loss_Curve.Last_Element <
           Self.Attributes.Best_Loss then
            Self.Attributes.Best_Loss :=
              Self.Attributes.Loss_Curve.Last_Element;
         end if;
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
   --  L1125  MLPClassifier._validate_input
   function Validate_Input (Self        : in out MLP_Classifier; Y : Integer_Matrix;
                            Incremental : Boolean) return Boolean_Matrix is
      use Ada.Containers;
      use NL_Types.Integer_Package;
      Routine_Name : constant String :=
                       "Multilayer_Perceptron.Validate_Input ";
      Classes      : NL_Types.Integer_List;
      Binarizer    : Label.Label_Binarizer;
   begin
      if Self.Attributes.Classes.Is_Empty and then
        Self.Parameters.Warm_Start and then Incremental then
         Classes := Multiclass_Utils.Unique_Labels (Y);
         if Self.Parameters.Warm_Start then
            Assert (Classes = Self.Attributes.Classes,
                    Routine_Name & "warm_start cannot be used if Y has" &
                      " different classes as in the previous call to fit.");
         end if;
         Assert (Classes.Length = Self.Attributes.Classes.Length,
                 Routine_Name & "Y and  Self.Classes do not have the same"
                 & "number of classes.");
      else
         Label.Fit (Binarizer, Y);
         Self.Attributes.Binarizer := Binarizer;
         Self.Attributes.Classes := Self.Attributes.Binarizer.Classes;
      end if;

      --        Put_Line (Routine_Name & "Binarizer.Y_Kind: " &
      --                    Multiclass_Utils.Y_Type'Image (Binarizer.Y_Kind));

      return Label.Label_Binarize (Flatten (Y), Self.Attributes.Classes);

   end Validate_Input;

   --  -------------------------------------------------------------------------

end Multilayer_Perceptron;
