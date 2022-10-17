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
with Ada.Containers;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;

with Maths;

with Base;
--  with Data_Splitter;
with Multiclass_Utils;
with Neural_Maths;
with Optimise;
with Opt_Minimise;
with Shuffler;
with Test_Support;
with Utils;
with Utils_Optimise;

package body Multilayer_Perceptron is

   First_Pass : Boolean := True;

   function Compute_Loss_Gradient
     (Self          : MLP_Classifier;
      Layer         : Positive;
      Num_Samples   : Positive;
      Activations   : Real_Matrix_List;
      Deltas        : Real_Matrix_List) return Parameters_Record;
   procedure Fit_Lbfgs (Self         : in out MLP_Classifier;
                        X            : Real_Float_Matrix;
                        Y            : Binary_Matrix;
                        Gradients    : in out Parameters_List;
                        Activations  : in out Real_Matrix_List);
   --  Deltas       : Real_Matrix_List;
   --  Layer_Units  : Integer_List);
   procedure Fit_Stochastic (Self         : in out MLP_Classifier;
                             X            : Real_Float_Matrix;
                             Y            : Binary_Matrix;
                             Gradients    : in out Parameters_List;
                             Incremental  : Boolean := False);
   procedure Forward_Pass (Self         : MLP_Classifier;
                           Activations  : in out Real_Matrix_List);
   function Forward_Pass_Fast (Self      : MLP_Classifier;
                               X         : Real_Float_Matrix)
                               return Real_Float_Matrix;
   procedure Initialize (Self        : in out MLP_Classifier;
                         Layer_Units : Integer_List);
   function Init_Coeff (Self            : in out MLP_Classifier;
                        Fan_In, Fan_Out : Positive) return Parameters_Record;
   procedure Process_Batch (Self             : in out MLP_Classifier;
                            X                : Real_Float_Matrix;
                            Y                : Binary_Matrix;
                            Params           : in out Parameters_List;
                            Gradients        : in out Parameters_List;
                            Batch_Slice      : Slice_Record;
                            Batch_Size       : Positive;
                            Accumulated_Loss : in out Float);
   procedure Update_No_Improvement_Count
     (MLP   : in out MLP_Classifier; Early_Stopping : Boolean;
      X_Val : Real_Float_Matrix; Y_Val : Binary_Matrix);
   procedure Update_Hidden_Layer_Gradients
     (MLP         : MLP_Classifier;
      Activations : Real_Matrix_List;
      Deltas      : in out Real_Matrix_List;
      Gradients   : in out Parameters_List;
      Num_Samples : Positive);
   procedure Validate_Hyperparameters (Self : MLP_Classifier);

   --  -------------------------------------------------------------------------

   function "-" (L, R : Loss_Grad_Result) return Loss_Grad_Result is
      Result : Loss_Grad_Result := L;
   begin
      Result.Loss := Result.Loss - R.Loss;
      for index in Result.Parameters.First_Index ..
        Result.Parameters.Last_Index loop
         Result.Parameters (index) :=
           Result.Parameters (index) - R.Parameters (index);
      end loop;

      return Result;

   end "-";

   --  -------------------------------------------------------------------------

   function "/" (L : Loss_Grad_Result; R : Float) return Loss_Grad_Result is
      Recip_R : constant Float := 1.0 / R;
      Result : Loss_Grad_Result := L;
   begin
      Result.Loss := Recip_R * Result.Loss;
      for index in Result.Parameters.First_Index ..
        Result.Parameters.Last_Index loop
         Result.Parameters (index) := Recip_R * Result.Parameters (index);
      end loop;

      return Result;

   end "/";

   --  -------------------------------------------------------------------------
   --  L241  Backprop computes the MLP loss function and its derivatives
   --       with respect to each parameter: weights and bias vectors.
   --  Activations contains an activation matrix for each layer
   function Backprop (MLP               : in out MLP_Classifier;
                      X                 : Real_Float_Matrix;
                      Y                 : Binary_Matrix;
                      Activations       : in out Real_Matrix_List;
                      Loss              : out Float) return Parameters_List is
      use Ada.Containers;
      use Base_Neural;
      use Parameters_Package;
      use Real_Float_Arrays;
      use Real_Matrix_List_Package;
      Routine_Name       : constant String :=
                             "Multilayer_Perceptron.Backprop ";
      Num_Samples        : constant Positive := Positive (X'Length);
      Y_Float            : constant Real_Float_Matrix :=
                             To_Real_Float_Matrix (Y);
      Loss_Function_Name : Loss_Function_Type;
      Deltas             : Real_Matrix_List;
      Sum_Sq_Coeffs      : Float := 0.0;
      Updated_Gradients  : Parameters_List;
   begin
      --  L284
      Assert (Y_Float'Length = Activations.Last_Element'Length,
              Routine_Name & "L284 unequal Y_Float and Activations lengths");
      Assert (Y_Float'Length (2) = Activations.Last_Element'Length (2),
              Routine_Name & "L284 Y_Float has different number of columns" &
                Integer'Image (Y_Float'Length (2)) & " to" &
                " Activations.Last_Element columns" &
                Integer'Image (Activations.Last_Element'Length (2)));

      if MLP.Attributes.Loss_Function_Name = Log_Loss_Function and then
        MLP.Attributes.Out_Activation = Logistic_Activation then
         Loss_Function_Name := Binary_Log_Loss_Function;
      else
         Loss_Function_Name := MLP.Attributes.Loss_Function_Name;
      end if;

      case Loss_Function_Name is
         when Binary_Log_Loss_Function =>
            Loss := Binary_Log_Loss (Y_Float, Activations.Last_Element);
         when Log_Loss_Function =>
            Loss := Log_Loss (Y_Float, Activations.Last_Element);
         when Squared_Error_Function =>
            Loss := Squared_Loss (Y_Float, Activations.Last_Element);
      end case;

      --  L292  Add L2 regularization term to loss
      --  for s in self.coefs_:
      for layer in MLP.Attributes.Params.First_Index ..
        MLP.Attributes.Params.Last_Index loop
         declare
            Coeffs : constant Real_Float_Matrix :=
                       MLP.Attributes.Params (Layer).Coeff_Gradients;
         begin
            for row in Coeffs'Range loop
               for col in Coeffs'Range (2) loop
                  Sum_Sq_Coeffs := Sum_Sq_Coeffs + Coeffs (row, col) ** 2;
               end loop;
            end loop;
         end;  --  declare
      end loop;

      --  L292
      Loss := Loss + 0.5 * (MLP.Parameters.Alpha *
                              Sum_Sq_Coeffs / Float (Num_Samples));

      --  L297 Backward propagate
      --  The calculation of delta[last]  works with the following
      --  combinations of output activation and loss function:
      --  sigmoid and binary cross entropy, softmax and categorical cross
      --  entropy and identity with squared loss.
      --  The ith element of deltas holds the difference between the
      --  activations of the i + 1 layer and the backpropagated error.
      --  Deltas are gradients of loss with respect to z in each layer.
      --  The ith element of Activations (batch samples x classes) is a list
      --  of the ith layer class values of a batch.

      --  L301  Initialize Deltas
      Deltas.Set_Length (Count_Type (MLP.Attributes.N_Layers - 1));
      Assert (Activations.Last_Element'Length (2) = Y_Float'Length (2),
              Routine_Name & "L301 last Activations item width" &
                Integer'Image (Activations.Last_Element'Length (2)) &
                " differs from Y_Float width" &
                Integer'Image (Y_Float'Length (2)));
      --  L301  310
      Deltas.Replace_Element (Deltas.Last_Index,
                              Activations.Last_Element - Y_Float);
      Updated_Gradients.Set_Length (Count_Type (MLP.Attributes.N_Layers - 1));

      --  L304 Compute gradient for the last layer
      declare
         Grad : constant Parameters_Record :=
                  Compute_Loss_Gradient (MLP, MLP.Attributes.N_Layers - 1,
                                         Num_Samples, Activations, Deltas);
      begin
         Updated_Gradients.Replace_Element
           (Updated_Gradients.Last_Index, Grad);
      end;

      --  L310, L308
      Update_Hidden_Layer_Gradients
        (MLP, Activations, Deltas, Updated_Gradients, Num_Samples);

      return Updated_Gradients;

   end Backprop;

   --  -------------------------------------------------------------------------
   --  scikit-learn/sklearn/utils/multiclass.py
   --  L340  Check_Partial_Fit_First_Call returns True if this was the first
   --  call to partial_fit on clf in which case the classes attribute
   --  is also set on clf.
   --  Implemented in Multilayer_Perceptron to avoid circular references
   function Check_Partial_Fit_First_Call
     (Self : in out MLP_Classifier; Classes : Integer_List) return Boolean is
      use Integer_Package;
      Routine_Name : constant String :=
                       "Multilayer_Perceptron.Check_Partial_Fit_First_Call ";
      Result       : Boolean := False;
   begin
      Assert (not Is_Empty (Self.Attributes.Classes) or not Is_Empty (Classes),
              Routine_Name &
                "Classes must be passed on the first call to Partial_Fit.");

      if not Is_Empty (Classes) then
         if not Self.Attributes.Classes.Is_Empty then
            Assert  (Self.Attributes.Classes = Classes, Routine_Name &
                       "Classes is not the same as on last call to partial_fit");
         else
            --  This is the first call to partial_fit
            Self.Attributes.Classes := Multiclass_Utils.Unique_Labels (Classes);
            Result := True;
         end if;

      end if;

      return Result;

   end Check_Partial_Fit_First_Call;

   --  -------------------------------------------------------------------------

   procedure Check_Weights (Self : MLP_Classifier) is
      Routine_Name : constant String :=
                       "Multilayer_Perceptron.Check_Weights ";
      Weight       : Float;
      Bad          : Boolean := False;
   begin
      for Param_Cursor of Self.Attributes.Params loop
         declare
            Params : constant Parameters_Record := Param_Cursor;
         begin
            for row in Params.Coeff_Gradients'Range loop
               for col in Params.Coeff_Gradients'Range (2) loop
                  Weight := Params.Coeff_Gradients (row, col);
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
   function C_Init (Hidden_Layer_Sizes  : Integer_List :=
                      Integer_Package.Empty_Vector;
                    Activation          : Base_Neural.Activation_Type :=
                      Base_Neural.Rect_LU_Activation;
                    Solver              : Solver_Type := Adam_Solver;
                    Alpha               : Float := 0.0001;
                    Batch_Size          : Positive := 200;
                    Learning_Rate       : Float := 0.001;
                    Learning_Rate_Init  : Float := 0.001;
                    Power_T             : Float := 0.5;
                    Max_Iter            : Natural := 200;
                    Loss_Function_Name  : Loss_Function_Type :=
                      Log_Loss_Function;
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
                    Max_Fun             : Max_Function_Access := null;
                    RF_Fun              : Num_Diff.Deriv_Float_Fun_Access
                    := null)
                    return MLP_Classifier is
      Classifier : MLP_Classifier;
   begin
      Classifier.Parameters.Hidden_Layer_Sizes := Hidden_Layer_Sizes;
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
      Classifier.Parameters.RF_Fun              := RF_Fun;
      First_Pass := True;

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
   --  Compute_Loss_Gradient does backpropagation for the specified one layer.
   function Compute_Loss_Gradient
     (Self          : MLP_Classifier;
      Layer         : Positive;
      Num_Samples   : Positive;
      Activations   : Real_Matrix_List;
      Deltas        : Real_Matrix_List) return Parameters_Record is
      use Real_Float_Arrays;
      --        Routine_Name        : constant String :=
      --                                "Multilayer_Perceptron.Compute_Loss_Gradient ";
      --  The ith element of Deltas holds the difference between the
      --  activations of the i + 1 layer and the backpropagated error.
      --  An activation converts the output from a layer into a form that is
      --  suitable for input into the next layer
      --  Coeff_Grads is a list of fan_in x fan_out matrices
      --  L185
      New_Coeff_Gradients : Real_Float_Matrix :=
                              Transpose (Activations (Layer)) * Deltas (Layer);
      --  Mean computes mean of values along the specified axis.
      New_Intercept_Grads : constant Real_Float_Vector :=
                              Neural_Maths.Mean (Deltas (Layer));
      New_Gradients       : Parameters_Record (New_Coeff_Gradients'Length,
                                               New_Coeff_Gradients'Length (2));
   begin
      --        Test_Support.Print_Float_Vector (Routine_Name & "Intercept_Grads",
      --                            Self.Attributes.Params (layer).Intercept_Grads, 1, 2);
      New_Coeff_Gradients :=
        (New_Coeff_Gradients + Self.Parameters.Alpha *
           Self.Attributes.Params (layer).Coeff_Gradients) /
          Float (Num_Samples);
      --  L194
      New_Gradients.Coeff_Gradients := New_Coeff_Gradients;
      New_Gradients.Intercept_Grads := New_Intercept_Grads;
      --        Test_Support.Print_Float_Vector (Routine_Name & "updated Intercept_Grads",
      --                                         New_Gradients.Intercept_Grads, 1, 2);
      return New_Gradients;

   end  Compute_Loss_Gradient;

   --  -------------------------------------------------------------------------

   procedure Fit (Self  : in out MLP_Classifier;
                  X     : Real_Float_Matrix;
                  Y     : Binary_Matrix; Incremental : Boolean := False) is
      use Ada.Containers;
      Routine_Name       : constant String :=
                             "Multilayer_Perceptron.Fit Binary Y ";
      Num_Features       : constant Positive := Positive (X'Length (2));
      Hidden_Layer_Sizes : constant Integer_List :=
                             Self.Parameters.Hidden_Layer_Sizes;
      --  L394
      Y_Bin              : constant Binary_Matrix :=
                             Validate_Input (Self, Y, Incremental);
      Layer_Units        : Integer_List;
      Activations        : Real_Matrix_List;
      Gradients          : Parameters_List;
   begin
      --  L385
      Validate_Hyperparameters (Self);
      First_Pass :=
        Self.Attributes.Params.Is_Empty or else
        (not Self.Parameters.Warm_Start and then not Incremental);

      --  L402
      Self.Attributes.N_Outputs := Positive (Y_Bin'Length (2));

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
         Test_Support.Print_Integer_List (Routine_Name & "L409 Layer_Units",
                                          Layer_Units);
      end if;

      Activations.Append (X);

      --  L417 Initialized grads are empty vectors, no initialization needed.
      --  L427
      if Self.Parameters.Solver = Sgd_Solver or else
        Self.Parameters.Solver = Adam_Solver then
         Fit_Stochastic (Self, X, Y_Bin, Gradients, Incremental);

         --  L444
      elsif Self.Parameters.Solver = Lbfgs_Solver then
         Fit_Lbfgs (Self, X, Y_Bin, Gradients, Activations);
      end if;

      Check_Weights (Self);

   end  Fit;

   --  -------------------------------------------------------------------------
   --  L377  MultilayerPerceptron._Fit
   --  L759  MultilayerPerceptron.Fit
   --  The Y_Bin values are class labels in classification,
   --  real numbers in regression.
   procedure Fit (Self        : in out MLP_Classifier;
                  X           : Real_Float_Matrix;
                  Y           : Integer_Matrix;
                  Incremental : Boolean := False) is
      use Ada.Containers;
      --        Routine_Name       : constant String :=
      --                               "Multilayer_Perceptron.Fit Integer Y ";
      Num_Features       : constant Positive := Positive (X'Length (2));
      Hidden_Layer_Sizes : constant Integer_List :=
                             Self.Parameters.Hidden_Layer_Sizes;
      --  L394  Y_Bin has dimensions num_samples x num_classes
      --  Validate_Input generates class columns from the Y columns
      Y_Bin              : constant Binary_Matrix :=
                             Validate_Input (Self, Y, Incremental);
      Layer_Units        : Integer_List;
      Activations        : Real_Matrix_List;
   begin
      --  L385
      Validate_Hyperparameters (Self);
      --        Test_Support.Print_Float_Matrix (Routine_Name & "X", X, 1, 2, 1, 5);
      --        Test_Support.Print_Integer_Matrix (Routine_Name & "Y", Y, 1, 5);
      --        Test_Support.Print_Binary_Matrix (Routine_Name & "Y_Bin", Y_Bin, 1, 5);
      First_Pass :=
        Self.Attributes.Params.Is_Empty or else
        (not Self.Parameters.Warm_Start and then not Incremental);

      --  L402  In this context "Outputs" are "classes" having binary values
      --  0 or 1 for each sample depending whether or not the sample output
      --  value corresponds to th class value
      Self.Attributes.N_Outputs := Positive (Y_Bin'Length (2));
      --  layer_units = [n_features] + hidden_layer_sizes + [self.n_outputs_]
      Layer_Units.Append (Num_Features);
      if Hidden_Layer_Sizes.Length > 0 then
         for index in Hidden_Layer_Sizes.First_Index ..
           Hidden_Layer_Sizes.Last_Index loop
            Layer_Units.Append (Hidden_Layer_Sizes.Element (index));
         end loop;
      end if;
      --  Add last layer
      Layer_Units.Append (Self.Attributes.N_Outputs);

      --  L409
      if First_Pass then
         Initialize (Self, Layer_Units);
      end if;

      Activations.Clear;
      Activations.Append (X);

      --  L417 Initialized grads are empty vectors, no initialization needed.
      --  L427
      if Self.Parameters.Solver = Sgd_Solver or else
        Self.Parameters.Solver = Adam_Solver then
         Fit_Stochastic (Self, X, Y_Bin, Self.Attributes.Params, Incremental);

         --  L444
      elsif Self.Parameters.Solver = Lbfgs_Solver then
         Fit_Lbfgs (Self, X, Y_Bin, Self.Attributes.Params, Activations);
      end if;

      Check_Weights (Self);

   end Fit;

   --  -------------------------------------------------------------------------
   --  L516
   procedure Fit_Lbfgs (Self            : in out MLP_Classifier;
                        X               : Real_Float_Matrix;
                        Y               : Binary_Matrix;
                        Gradients       : in out Parameters_List;
                        Activations     : in out Real_Matrix_List) is
      --  Deltas          : in out Real_Matrix_List;
      --  Layer_Units     : Integer_List) is
      --        Routine_Name : constant String := "Multilayer_Perceptron.Fit_Lbfgs ";
      --        Num_Samples  : constant Positive := Positive (X'Length);
      --        Start        : Positive := 1;
      --        Last         : Positive;
      --        N_Fan_In     : Positive;
      --        N_Fan_Out    : Positive;
      --        Options      : constant Opt_Minimise.Minimise_Options :=
      --     (Self.Parameters.Max_Fun, Self.Parameters.Max_Iter,
      --      Self.Parameters.Tol, 20);
      --  Grads        : Parameters_List := Self.Attributes.Params;
      --  Opt_Result is the optimization result represented as a Optimise_Result
      --  record.
      --  Important attributes are: the solution array X, a Boolean flag
      --  Success indicating if the optimizer exited successfully

      Args         : Loss_Grad_Args (X'Length, X'Length (2), Y'Length (2));
   begin
      Args.Self := Self;
      Args.Params := Gradients;
      Args.X := X;
      Args.Y := Y;
      Args.Activations := Activations;
      Args.Gradients := Gradients;

      --  L546  Gradients is similar to packed_coef_inter
      declare
         Opt_Result : constant Optimise.Optimise_Result
           := Opt_Minimise.Minimise
             (Fun => Loss_Grad_LBFGS'Access, Args => Args, X0 => Gradients,
              Method => Opt_Minimise.L_BFGS_B_Method,
              Jac => Num_Diff.FD_True);
      begin
         Self.Attributes.N_Iter :=
           Utils_Optimise.Check_Optimize_Result (Opt_Result,
                                                 Self.Parameters.Max_Iter);
         --  L566
         Self.Attributes.Loss := Opt_Result.Fun.Loss;

      end;

   end Fit_Lbfgs;

   --  -------------------------------------------------------------------------

   --  L563
   procedure Fit_Stochastic (Self        : in out MLP_Classifier;
                             X           : Real_Float_Matrix;
                             Y           : Binary_Matrix;
                             Gradients   : in out Parameters_List;
                             Incremental : Boolean := False) is
      Routine_Name     : constant String :=
                           "Multilayer_Perceptron.Fit_Stochastic ";
      --        Is_Classifier  : constant Boolean :=
      --       Self.Estimator_Kind = Classifier_Estimator;
      Num_Samples       : constant Positive := Positive (X'Length);
      Recip_Num_Samples : constant Float := 1.0 / Float (Num_Samples);
      Params            : Parameters_List := Self.Attributes.Params;
      Iter              : Natural := 0;
      Continue          : Boolean := True;
      Early_Stopping    : Boolean := Self.Parameters.Early_Stopping;
      Batch_Size        : Positive;
      Batches           : Slices_List;
      Accumulated_Loss  : Float := 0.0;
      Msg               : Unbounded_String;
      Is_Stopping       : Boolean := False;
   begin
      Self.Attributes.Loss_Curve.Clear;
      Early_Stopping := Early_Stopping and then not Incremental;
      --  L576
      if not Incremental or else
        Self.Attributes.Optimizer.Kind = No_Optimizer then
         Init_Optimizer (Self);
      end if;

      --  L597
      if Early_Stopping then
         Assert (False, Routine_Name & "L597  Early_Stopping not coded!");
         --  Put_Line (Routine_Name & "L597  *** Early_Stopping ***");
         --              Should_Stratify := Is_Classifier and Self.Attributes.N_Outputs = 1;
         --              if Should_Stratify then
         --                  Stratify := Y;
         --              end if;
         --  Data_Splitter.Train_Test_Split
         --    (X => X, Y => Y,
         --     Train_Size => Train_Size, Test_Size  => Test_Size,
         --     Train_X => Train_X, Train_Y => Train_Y,
         --     Test_X  => Test_X, Test_Y => Test_Y);

         --           if Is_Classifier then
         --              Val_Y := Label.Inverse_Transform (LE_U, Test_Y);
         --           end if;
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
         --  Shuffling done in Process_Batch
         Accumulated_Loss := 0.0;
         --  Batch_Iter := 0;  Batch_Iter NOT USED
         --  L636
         for Batch_index in Batches.First_Index ..
           Batches.Last_Index loop
            Process_Batch (Self, X, Y, Params, Gradients, Batches (Batch_Index),
                           Batch_Size, Accumulated_Loss);
         end loop;

         --  L661
         Self.Attributes.N_Iter := Self.Attributes.N_Iter + 1;
         Self.Attributes.Loss := Recip_Num_Samples * Accumulated_Loss;
         Self.Attributes.T := Self.Attributes.T + Num_Samples;
         Self.Attributes.Loss_Curve.Append (Self.Attributes.Loss);
         --  L668
         if Self.Parameters.Verbose then
            Put_Line (Routine_Name & "Iteration" &
                        Integer'Image (Self.Attributes.N_Iter) &
                        ", loss = " & Float'Image (Self.Attributes.Loss));
         end if;

         --  L669 Update no_improvement_count based on training loss or
         --       validation score according to early_stopping
         Update_No_Improvement_Count (Self, Early_Stopping, X, Y);
         --  for learning rate that needs to be updated at iteration end;
         if Self.Attributes.Optimizer.Kind = Optimizer_SGD then
            Iteration_Ends (Self.Attributes.Optimizer.SGD, Self.Attributes.T);
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

         if not Incremental and
           Self.Attributes.N_Iter = Self.Parameters.Max_Iter then
            Put_Line (Routine_Name & "Maximum iterations (" &
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
         Put_Line (Routine_Name & "Number of iterations: "
                   & Integer'Image (Iter) & "  loss: " &
                     Float'Image (Self.Attributes.Loss));
      end if;

   end Fit_Stochastic;

   --  -------------------------------------------------------------------------
   --  L119  Forward_Pass performs a forward pass on the neural network by
   --  computing the activation values of the neurons in the hidden layers and
   --  the output layer.
   procedure Forward_Pass (Self        : MLP_Classifier;
                           Activations : in out Real_Matrix_List) is
      --  Activations: layers x samples x features
      --  The ith element of Activations (length n_layers - 1) holds the
      --  activation values of the ith layer.
      --        use Ada.Containers;
      use Base_Neural;
      use Parameters_Package;
      Routine_Name      : constant String :=
                            "Multilayer_Perceptron.Forward_Pass ";
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
            Params              : constant Parameters_Record :=
                                    Self.Attributes.Params (layer);
            --  L131
            Updated_Activ_Grads : constant Real_Float_Matrix
              := Activations (layer) * Params.Coeff_Gradients;
         begin

            --  L132 Add layer + 1 activation
            Activations.Append (Updated_Activ_Grads + Params.Intercept_Grads);
            Assert (Activations.Last_Index = layer + 1, Routine_Name &
                      "L132 Activations.Last_Index"
                    & Integer'Image (Activations.Last_Index) &
                      " /= layer + 1:" & Integer'Image (layer + 1));

            --  L134 For the hidden layers
            if layer /= Num_Layers - 1 then
               case Hidden_Activation is
                  when Identity_Activation => null;
                  when Logistic_Activation =>
                     Logistic (Activations (Activations.Last_Index));
                  when Tanh_Activation =>
                     Tanh (Activations (Activations.Last_Index));
                  when Rect_LU_Activation =>
                     Rect_LU (Activations (Activations.Last_Index));
                  when Softmax_Activation =>
                     Softmax (Activations (Activations.Last_Index));
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
         when Rect_LU_Activation =>
            Rect_LU (Activations (Activations.Last_Index));
         when Softmax_Activation =>
            Softmax (Activations (Activations.Last_Index));
      end case;

      if Activations.Last_Element'Length (2) > 1 then
         --  Check that Activations.Last_Element rows are probabilities
         Is_Probilities_Matrix (Routine_Name & "final Activations.Last_Element ",
                                Activations.Last_Element);
      end if;

   end Forward_Pass;

   --  -------------------------------------------------------------------------
   --  L144 Forward_Pass_Fast predict using the trained model
   --
   --  It is the same as Forward_Pass but does not record all layer activations
   --  and returns only the last layer's activation of size
   --  n_samples x n_outputs
   function Forward_Pass_Fast (Self : MLP_Classifier; X : Real_Float_Matrix)
                               return Real_Float_Matrix is
      use Base_Neural;
      use Real_Float_Arrays;
      use Parameters_Package;
      type Activations_Array is array (Integer range <>) of Real_Float_List;
      Routine_Name : constant String :=
                       "Multilayer_Perceptron.Forward_Pass_Fast ";

      function To_Activations_Array (Activations : Real_Float_Matrix)
                                     return Activations_Array is
         Result   : Activations_Array (Activations'Range);
      begin
         for row in Activations'Range loop
            for col in Activations'Range (2) loop
               Result (row).Append (Activations (row, col));
            end loop;
         end loop;

         return Result;

      end To_Activations_Array;

      function To_Matrix (Activations : Activations_Array)
                          return Real_Float_Matrix is
         Row_Data : Real_Float_List;
         Result   : Real_Float_Matrix (Activations'Range, 1 ..
                                         Integer (Activations (1).Length));
      begin
         for row in Result'Range loop
            Row_Data := Activations (row);
            for col in Result'Range (2) loop
               Result (row, col) := Row_Data (col);
            end loop;
         end loop;

         return Result;

      end To_Matrix;

      Hidden_Activation : constant Activation_Type :=
                            Self.Parameters.Activation;
      Output_Activation : constant Activation_Type :=
                            Self.Attributes.Out_Activation;
      Num_Layers        : constant Positive := Self.Attributes.N_Layers;
      --  Activations_Array used to allow for different sized matrices
      --  L160 Initialize first layer
      Activations       : Activations_Array := To_Activations_Array (X);
   begin
      --  L167 Forward propagate
      --  python range(self.n_layers_ - 1) = 0 .. self.n_layers_ - 1
      for layer in 1 .. Num_Layers - 1 loop
         declare
            Params             : constant Parameters_Record :=
                                   Self.Attributes.Params (layer);
            Activations_Matrix : constant Real_Float_Matrix :=
                                   To_Matrix (Activations);
            Updated_Activation : Real_Float_Matrix
              := Activations_Matrix * Params.Coeff_Gradients;
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

            Activations := To_Activations_Array (Updated_Activation);
         end;
      end loop;

      declare
         Activ_Out  : Real_Float_Matrix := To_Matrix (Activations);
      begin
         --  L172
         case Output_Activation is
            when Identity_Activation => null;
            when Logistic_Activation => Logistic (Activ_Out);
            when Tanh_Activation => Tanh (Activ_Out);
            when Rect_LU_Activation => Rect_LU (Activ_Out);
            when Softmax_Activation => Softmax (Activ_Out);
         end case;

         Is_Probilities_Matrix (Routine_Name & "Activ_Out", Activ_Out);
         return Activ_Out;
      end;

   end Forward_Pass_Fast;

   --  -------------------------------------------------------------------------
   --  L360  MultilayerPerceptron._init_coef
   function Init_Coeff (Self            : in out MLP_Classifier;
                        Fan_In, Fan_Out : Positive) return Parameters_Record is
      use Ada.Numerics.Float_Random;
      use Maths;
      use Maths.Float_Math_Functions;
      use Base_Neural;
--        Routine_Name : constant String := "Multilayer_Perceptron.Init_Coeff ";
      Params       : Parameters_Record (Fan_In, Fan_Out);
      Factor       : Float;
      Init_Bound   : Float;
      Rand_Float   : Float;
   begin
      if Self.Parameters.Activation = Logistic_Activation then
         Factor := 2.0;
      else
         Factor := 6.0;
      end if;

      Init_Bound := Sqrt (Factor / Float (Fan_In + Fan_Out));
      --  Generate random weights, Random_Float -1.0 .. 1.0
      for f_in in 1 .. Fan_In loop
         for f_out in 1 .. Fan_Out loop
            Rand_Float := Random (Float_Gen);
            Params.Coeff_Gradients (f_in, f_out) :=
              Init_Bound * (2.0 * Rand_Float - 1.0);
--              Put_Line (Routine_Name & "" & Float'Image (Rand_Float));
         end loop;
      end loop;

      --  Generate random bias
      for f_out in 1 .. Fan_Out loop
         Params.Intercept_Grads (f_out) :=
           Init_Bound * (2.0 * Random (Float_Gen) - 1.0);
      end loop;

      return Params;

   end Init_Coeff;

   --  ------------------------------------------------------------------------
   --  L320  BaseMultilayerPerceptron._initialize
   procedure Initialize (Self        : in out MLP_Classifier;
                         Layer_Units : Integer_List) is
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
      Self.Attributes.Params.Clear;  --  Layers

      --  L351
      --  python range(self.n_layers_ - 1) => 0 .. self.n_layers_ - 1
      for layer in 1 .. Self.Attributes.N_Layers - 1 loop
         --  Add coefficent matrices and intercept vectors for layer.
         Fan_In := Layer_Units (layer);
         Fan_Out := Layer_Units (layer + 1);
         Self.Attributes.Params.Append (Init_Coeff (Self, Fan_In, Fan_Out));
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

   procedure Is_Probilities_Matrix (Msg : String; PM : Real_Float_Matrix) is
      Routine_Name : constant String :=
                       "Multilayer_Perceptron.Is_Probilities_Matrix, ";
      Sum          : Real_Float_Vector (PM'Range) := (others => 0.0);
   begin
      for row in PM'Range loop
         for col in PM'Range (2) loop
            Assert (PM (row, col) >= 0.0, Routine_Name & Msg & "Matrix" &
                      Integer'Image (row) & "," & Integer'Image (col) & " = " &
                      Float'Image (PM (row, col)));
            Sum (row) := Sum (row) + PM (row, col);
         end loop;
      end loop;

      if PM'Length (2) > 1 then
         for row in Sum'Range loop
            Assert (Sum (row) >= 1.0 - 10.0 ** (-6) and
                      Sum (row) <= 1.0 + 10.0 ** (-6), Routine_Name & Msg &
                      " Total probability for row (" & Integer'Image (row) &
                      ") " & Float'Image (Sum (row)) & " is not close to 1.0");
         end loop;
      end if;

   end Is_Probilities_Matrix;

   --  -------------------------------------------------------------------------

   function Loss_Grad_LBFGS (Args : Loss_Grad_Args) return Loss_Grad_Result is
      Self        : MLP_Classifier := Args.Self;
      Params      : Parameters_List := Args.Params;
      Activations : Real_Matrix_List := Args.Activations;
      Loss        : Float := Float'Safe_Last;
   begin
      --  L237 _backprop
      Forward_Pass (Self, Activations);
      Params := Backprop (Self, Args.X, Args.Y, Activations, Loss);

      return (Loss, Params);

   end Loss_Grad_LBFGS;

   --  -------------------------------------------------------------------------
   --  L778  Partial_Fit updates the model with a single iteration over the
   --        given data.
   procedure Partial_Fit (Self : in out MLP_Classifier; X : Real_Float_Matrix;
                          Y    : Binary_Matrix) is
      --        Routine_Name : constant String := "Multilayer_Perceptron.Partial_Fit 1 ";
   begin
      Fit (Self, X, Y, Incremental => True);

   end Partial_Fit;

   --  -------------------------------------------------------------------------
   --  L778  Partial_Fit updates the model with a single iteration over the
   --        given data.
   procedure Partial_Fit (Self : in out MLP_Classifier; X : Real_Float_Matrix;
                          Y    : Integer_Matrix) is
      --        Routine_Name : constant String := "Multilayer_Perceptron.Partial_Fit 1 ";
   begin
      Fit (Self, X, Y, Incremental => True);

   end Partial_Fit;

   --  -------------------------------------------------------------------------
   --  L1186  Partial_Fit updates the model with one iteration over the data
   --  Don't provide default for Classes to avoid ambiguity with
   --  Partial_Fit (X, Y)
   procedure  Partial_Fit
     (Self    : in out MLP_Classifier; X : Real_Float_Matrix;
      Y       : Integer_Matrix; Classes : Integer_List) is
      use Label;
      use Multiclass_Utils;
      LB           : Label_Binarizer;
      Routine_Name : constant String := "Multilayer_Perceptron.Partial_Fit 2 ";
   begin
      if Check_Partial_Fit_First_Call (Self, Classes) then
         Self.Attributes.Binarizer := LB;
         if Type_Of_Target (Y) = Y_Multilabel_Indicator then
            Put_Line (Routine_Name & "Y_Multilabel_Indicator");
            Fit (Self.Attributes.Binarizer, Y);
         else
            Put_Line (Routine_Name & "not Y_Multilabel_Indicator");
            Fit (Self.Attributes.Binarizer, Classes);
         end if;
      end if;

      Partial_Fit (Self, X, Y);

   end Partial_Fit;

   --  -------------------------------------------------------------------------
   --  L1186  Partial_Fit updates the model with one iteration over the data
   --  Don't provide default for Classes to avoid ambiguity with
   --  Partial_Fit (X, Y)
   procedure  Partial_Fit
     (Self    : in out MLP_Classifier; X : Real_Float_Matrix;
      Y       : Binary_Matrix; Classes : Integer_List) is
      use Label;
      use Multiclass_Utils;
      LB           : Label_Binarizer (Type_Of_Target (Y));
      Routine_Name : constant String :=
                       "Multilayer_Perceptron.Partial_Fit Binary ";
   begin
      if Check_Partial_Fit_First_Call (Self, Classes) then
         Self.Attributes.Binarizer := LB;
         if Type_Of_Target (Y) = Y_Multilabel_Indicator then
            Put_Line (Routine_Name & "Multilabel_Indicator");
            Fit (Self.Attributes.Binarizer, Y);
         else
            Put_Line (Routine_Name & "not Multilabel_Indicator");
            Fit (Self.Attributes.Binarizer, Classes);
         end if;
      end if;

      Partial_Fit (Self, X, Y);

   end Partial_Fit;

   --  -------------------------------------------------------------------------
   --  L1168
   function Predict (Self : MLP_Classifier; X : Real_Float_Matrix)
                     return Integer_Matrix is
      --        Routine_Name   : constant String := "Multilayer_Perceptron.Predict ";
      Y_Pred         : constant Real_Float_Matrix :=
                         Forward_Pass_Fast (Self, X);
   begin
      return Label.Inverse_Transform (Self.Attributes.Binarizer, Y_Pred);

   end Predict;

   --  -------------------------------------------------------------------------
   --  L1237  Probability estimates
   function Predict_ProbA (Self : MLP_Classifier; X : Real_Float_Matrix)
                           return Real_Float_Matrix is
      --        Routine_Name : constant String := "Multilayer_Perceptron.Predict_ProbA ";
      --  L1265
      Y_Pred       : constant Real_Float_Matrix := Forward_Pass_Fast (Self, X);
   begin
      --        if Self.Attributes.N_Outputs = 1 then
      --           declare
      --              Y_Pred_1  : Real_Float_Matrix
      --                (1 .. Y_Pred'Length * Y_Pred'Length (2), 1 .. 2);
      --           begin
      --              Test_Support.Print_Matrix_Dimensions (Routine_Name & "Y_Pred_1",
      --                                                    Y_Pred_1);
      --              for row in Y_Pred'Range loop
      --                 for col in Y_Pred'Range (2) loop
      --                    Y_Pred_1 (row - Y_Pred'First + col, 1) := Y_Pred (row, col);
      --                 end loop;
      --              end loop;
      --
      --              for row in Y_Pred_1'Range loop
      --                 Y_Pred_1 (row, 1) := 1.0 - Y_Pred (row, 1);
      --                 --                    Y_Pred_1 (row, 2) := 1.0 - Y_Pred (row, 1);
      --              end loop;
      --
      --           end;
      --
      --        else

      return Y_Pred;
      --        end if;

   end Predict_ProbA;

   --  -------------------------------------------------------------------------
   --  L637
   procedure Process_Batch (Self             : in out MLP_Classifier;
                            X                : Real_Float_Matrix;
                            Y                : Binary_Matrix;
                            Params           : in out Parameters_List;
                            Gradients        : in out Parameters_List;
                            Batch_Slice      : Slice_Record;
                            Batch_Size       : Positive;
                            Accumulated_Loss : in out Float) is
      --        Routine_Name : constant String :=
      --                                   "Multilayer_Perceptron.Process_Batch ";
      Num_Features : constant Positive := Positive (X'Length (2));
      --  X_Batch: samples x features
      X_Batch      : Real_Float_Matrix (1 .. Batch_Size, 1 .. Num_Features);
      Y_Batch      : Binary_Matrix (1 .. Batch_Size, Y'Range (2));
      --  Activations: layers x samples x features
      Activations  : Real_Matrix_List;
      Batch_Row    : Positive;
      Batch_Loss   : Float;
   begin
      --  Get batch data
      for row in Batch_Slice.First .. Batch_Slice.Last loop
         Batch_Row := row - Batch_Slice.First + 1;
         for col in 1 .. Num_Features loop
            X_Batch (Batch_Row, col) := X (row, col);
         end loop;

         for col in Y'Range (2) loop
            Y_Batch (Batch_Row, col) := Y (row, col);
         end loop;
      end loop;

      if Self.Parameters.Shuffle then
         Shuffler.Shuffle (X_Batch, Y_Batch);
      end if;

      --  L644  Initialize Activations
      --  Activations initialization checked OK
      Activations.Clear;
      Activations.Append (X_Batch);

      --  L645
      Forward_Pass (Self, Activations);
      --  Activations (1) checked OK after Forward_Pass
      --        Test_Support.Print_Float_Matrix
      --          (Routine_Name & "L645 Activations (last)",
      --           Activations (Activations.Last_Index), 1, 1);
      Gradients := Backprop (Self, X_Batch, Y_Batch, Activations, Batch_Loss);

      --  L665
      Accumulated_Loss := Accumulated_Loss + Batch_Loss *
        Float (Batch_Slice.Last - Batch_Slice.First + 1);
      --  L667 update weights
      --  Update_Params updates parameters with given gradients
      Stochastic_Optimizers.Update_Params
        (Self.Attributes.Optimizer, Params, Gradients);
      Self.Attributes.Params := Params;

   end Process_Batch;

   --  -------------------------------------------------------------------------

   procedure Update_Hidden_Layer_Gradients
     (MLP         : MLP_Classifier;
      Activations : Real_Matrix_List;
      Deltas      : in out Real_Matrix_List;
      Gradients   : in out Parameters_List;
      Num_Samples : Positive) is
      use Base_Neural;
      use Real_Float_Arrays;
      use Real_Matrix_List_Package;
      --        Routine_Name : constant String :=
      --                         "Multilayer_Perceptron.Update_Hidden_Layer_Gradients ";
   begin
      --  loop will be from MLP.Attributes.N_Layers - 1 down to 2
      for layer in reverse 2 .. MLP.Attributes.N_Layers - 1 loop
         declare
            Params : constant Parameters_Record :=
                       MLP.Attributes.Params (Layer);
         begin
            --  L311
            Deltas.Replace_Element (Layer - 1, Deltas.Element (Layer) *
                                      Transpose (Params.Coeff_Gradients));
            --  L312
            --  Activations (Layer) is the data computed by the logistic activation
            --  function during the forward pass.
            --  Deltas (Layer - 1) is the backpropagated error signal to be updated.
            case MLP.Parameters.Activation is
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
            Gradients.Replace_Element
              (layer - 1, Compute_Loss_Gradient
                 (Self => MLP, Layer => Layer - 1, Num_Samples => Num_Samples,
                  Activations => Activations, Deltas => Deltas));
         end;
      end loop;

   end Update_Hidden_Layer_Gradients;

   --  -------------------------------------------------------------------------
   --  L716
   procedure Update_No_Improvement_Count
     (MLP   : in out MLP_Classifier; Early_Stopping : Boolean;
      X_Val : Real_Float_Matrix; Y_Val : Binary_Matrix) is
      Routine_Name     : constant String
        := "Multilayer_Perceptron.Update_No_Improvement_Count ";
      Sample_Weight    : constant Real_Float_Vector (1 .. 0) := (others => 0.0);
      Last_Valid_Score : Float;
      Score_Val        : Float;
   begin
      if Early_Stopping then
         Score_Val := Base.Score (MLP, X_Val, To_Integer_Matrix (Y_Val),
                                  Sample_Weight);
         MLP.Parameters.Validation_Scores.Append (Score_Val);
         Last_Valid_Score := MLP.Parameters.Validation_Scores.Last_Element;
         if MLP.Parameters.Verbose then
            Put_Line (Routine_Name & "Validation score: " &
                        Float'Image (Last_Valid_Score));
         end if;

         --  L728
         if Last_Valid_Score <
           MLP.Parameters.Best_Validation_Score + MLP.Parameters.Tol then
            MLP.Attributes.No_Improvement_Count :=
              MLP.Attributes.No_Improvement_Count + 1;
         else
            MLP.Attributes.No_Improvement_Count := 0;
         end if;

         if Last_Valid_Score > MLP.Parameters.Best_Validation_Score then
            MLP.Parameters.Best_Validation_Score := Last_Valid_Score;
            MLP.Parameters.Best_Params := MLP.Attributes.Params;
         end if;

      else
         if MLP.Attributes.Loss_Curve.Last_Element >
           MLP.Attributes.Best_Loss - MLP.Parameters.Tol then
            MLP.Attributes.No_Improvement_Count :=
              MLP.Attributes.No_Improvement_Count + 1;
         else
            MLP.Attributes.No_Improvement_Count := 0;
         end if;

         if MLP.Attributes.Loss_Curve.Last_Element <
           MLP.Attributes.Best_Loss then
            MLP.Attributes.Best_Loss :=
              MLP.Attributes.Loss_Curve.Last_Element;
         end if;
      end if;

   end Update_No_Improvement_Count;

   --  -------------------------------------------------------------------------
   --  L455
   procedure Validate_Hyperparameters (Self : MLP_Classifier) is
      --           Incremental, Reset : Boolean) is
      --  Routine_Name : constant String := "Multilayer_Perceptron.Validate_Hyperparameters ";
   begin
      null;

   end Validate_Hyperparameters;

   --  -------------------------------------------------------------------------
   --  L1108  MLPClassifier._validate_input
   function Validate_Input (Self        : in out MLP_Classifier;
                            Y           : Binary_Matrix;
                            Incremental : Boolean) return Binary_Matrix is
      use Integer_Package;
      use Multiclass_Utils;
      Routine_Name : constant String :=
                       "Multilayer_Perceptron.Validate_Input Binary matrix ";
      Classes      : Integer_List;
      Binarizer    : Label.Label_Binarizer (Type_Of_Target (Y));
   begin
      if Self.Attributes.Classes.Is_Empty or else
        (not Self.Parameters.Warm_Start and not Incremental) then
         --  L1139
         Self.Attributes.Binarizer := Binarizer;
         Label.Fit (Self.Attributes.Binarizer, Y);
         Self.Attributes.Classes := Self.Attributes.Binarizer.Classes;
      else
         --  L1143
         Classes := Multiclass_Utils.Unique_Labels (Y);
         Self.Attributes.Binarizer.Classes := Classes;
         if Self.Parameters.Warm_Start then
            Assert (Classes = Self.Attributes.Classes,
                    Routine_Name & "warm_start cannot be used if Y has" &
                      " different classes as in the previous call to fit.");
         else
            for index in Classes.First_Index .. Classes.Last_Index loop
               Assert (Self.Attributes.Classes.Contains (Classes (index)),
                       Routine_Name & "Y has classes not in Self.Classes");
            end loop;
         end if;
      end if;

      --  Python code downcasts to bool to prevent upcasting when working with
      --  float32 data
      return Label.Transform (Self.Attributes.Binarizer, Y);

   end Validate_Input;

   --  -------------------------------------------------------------------------
   --  L1108  MLPClassifier._validate_input
   function Validate_Input (Self        : in out MLP_Classifier;
                            Y           : Integer_Matrix;
                            Incremental : Boolean) return Binary_Matrix is
      use Multiclass_Utils;
      use Integer_Package;
      Routine_Name : constant String :=
                       "Multilayer_Perceptron.Validate_Input Integer matrix ";
      Classes      : Integer_List;
      Binarizer    : Label.Label_Binarizer (Type_Of_Target (Y));
   begin
      if Self.Attributes.Classes.Is_Empty or else
        (not Self.Parameters.Warm_Start and not Incremental) then
         --  L1139
         Self.Attributes.Binarizer := Binarizer;
         Label.Fit (Self.Attributes.Binarizer, Y);
         Self.Attributes.Classes := Self.Attributes.Binarizer.Classes;
      else
         --  L1143
         Classes := Multiclass_Utils.Unique_Labels (Y);
         if Self.Parameters.Warm_Start then
            Assert (Classes = Self.Attributes.Classes,
                    Routine_Name & "warm_start cannot be used if Y has" &
                      " different classes as in the previous call to fit.");
         else
            for index in Classes.First_Index .. Classes.Last_Index loop
               Assert (Self.Attributes.Classes.Contains (Classes (index)),
                       Routine_Name & "Y has classes not in Self.Classes");
            end loop;
         end if;
      end if;

      --  Python code downcasts to bool to prevent upcasting when working with
      --  float32 data
      --        Test_Support.Print_Binary_Matrix
      --          (Routine_Name & "result", Label.Transform (Self.Attributes.Binarizer, Y), 1, 3);
      return Label.Transform (Self.Attributes.Binarizer, Y);

   end Validate_Input;

   --  -------------------------------------------------------------------------

end Multilayer_Perceptron;
