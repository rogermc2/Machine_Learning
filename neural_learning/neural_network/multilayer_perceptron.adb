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
with Utilities;

--  with Base;
--  with Data_Splitter;
with Multiclass_Utils;
with Neural_Maths;
with Optimise;
with Opt_Minimise;
with Printing;
with Utils;
with Utils_Optimise;

package body Multilayer_Perceptron is

   First_Pass : Boolean := True;

   procedure Compute_Loss_Gradient
     (Self          : MLP_Classifier;
      Layer         : Positive;
      Num_Samples   : Positive;
      Activations   : Real_Matrix_List;
      Deltas        : Real_Matrix_List;
      Gradients     : in out Parameters_List);
   procedure Fit_Lbfgs (Self         : in out MLP_Classifier;
                        X            : Real_Float_Matrix;
                        Y            : Binary_Matrix;
                        Activations  : in out Real_Matrix_List);
   --  Deltas       : Real_Matrix_List;
   --  Grads        : in out Parameters_List;
   --  Layer_Units  : Integer_List);
   procedure Fit_Stochastic (Self         : in out MLP_Classifier;
                             X            : Real_Float_Matrix;
                             Y            : Binary_Matrix;
                             Incremental  : Boolean := False);
   procedure Forward_Pass (Self         : MLP_Classifier;
                           Activations  : in out Real_Matrix_List);
   function Forward_Pass_Fast (Self  : MLP_Classifier; X : Real_Float_Matrix)
                               return Real_Float_Matrix;
   procedure Initialize (Self        : in out MLP_Classifier;
                         Layer_Units : Integer_List);
   function Init_Coeff (Self            : in out MLP_Classifier;
                        Fan_In, Fan_Out : Positive) return Parameters_Record;
   procedure Is_Probilities_Matrix (Msg : String; PM : Real_Float_Matrix);
   procedure Process_Batch (Self             : in out MLP_Classifier;
                            X                : Real_Float_Matrix;
                            Y                : Binary_Matrix;
                            Batch_Slice      : Slice_Record;
                            Batch_Size       : Positive;
                            Accumulated_Loss : in out Float);
   procedure Update_No_Improvement_Count
     (Self   : in out MLP_Classifier);
   --     procedure Update_No_Improvement_Count
   --       (Self   : in out MLP_Classifier; Early_Stopping : Boolean;
   --        X_Val  : Real_Float_Matrix; Y_Val : Integer_Matrix);
   procedure Update_Hidden_Layer_Gradients
     (Self               : in out MLP_Classifier;
      Activations        : Real_Matrix_List;
      Deltas             : in out Real_Matrix_List;
      Gradients          : in out Parameters_List;
      Layer, Num_Samples : Positive);
   procedure Validate_Hyperparameters (Self : MLP_Classifier);

   --  -------------------------------------------------------------------------

   function "-" (L, R : Loss_Grad_Result) return Loss_Grad_Result is
      Result : Loss_Grad_Result := L;
   begin
      Result.Loss := Result.Loss - R.Loss;
      for index in Result.Gradients.First_Index ..
        Result.Gradients.Last_Index loop
         Result.Gradients (index) :=
           Result.Gradients (index) - R.Gradients (index);
      end loop;

      return Result;

   end "-";

   --  -------------------------------------------------------------------------

   function "/" (L : Loss_Grad_Result; R : Float) return Loss_Grad_Result is
      Result : Loss_Grad_Result := L;
   begin
      Result.Loss := Result.Loss / R;
      for index in Result.Gradients.First_Index ..
        Result.Gradients.Last_Index loop
         Result.Gradients (index) := Result.Gradients (index) / R;
      end loop;

      return Result;

   end "/";

   --  -------------------------------------------------------------------------
   --  L241  Backprop computes the MLP loss function and its derivatives
   --       with respect to each parameter: weights and bias vectors.
   --  Activations contains an activation matrix for each layer
   procedure Backprop (Self        : in out MLP_Classifier;
                       X           : Real_Float_Matrix;
                       Y           : Binary_Matrix;
                       Activations : in out Real_Matrix_List;
                       Loss        : out Float;
                       Gradients   : out Parameters_List) is
      use Ada.Containers;
      use Base_Neural;
      use Parameters_Package;
      use Real_Float_Arrays;
      use Real_Matrix_List_Package;
      Routine_Name       : constant String :=
                             "Multilayer_Perceptron.Backprop ";
      Num_Samples        : constant Positive := Positive (X'Length);
      Y_Prob             : constant Real_Float_Matrix :=
                             To_Real_Float_Matrix (Y);
      Loss_Function_Name : Loss_Function_Type;
      Deltas             : Real_Matrix_List;
      Sum_Sq_Coeffs      : Float;
   begin
--        Printing.Print_Float_Matrix
--            (Routine_Name & "Y_Prob", Y_Prob, 1, 3);
      Is_Probilities_Matrix (Routine_Name & "Y_Prob ", Y_Prob);
      --  L284
      if Self.Attributes.Loss_Function_Name = Log_Loss_Function and then
        Self.Attributes.Out_Activation = Logistic_Activation then
         Loss_Function_Name := Binary_Log_Loss_Function;
      else
         Loss_Function_Name := Self.Attributes.Loss_Function_Name;
      end if;

--        Printing.Print_Float_Matrix
--            (Routine_Name & "L284+ Activations.Last_Element",
--             Activations.Last_Element);
      case Loss_Function_Name is
         when Binary_Log_Loss_Function =>
            Loss := Binary_Log_Loss (Y_Prob, Activations.Last_Element);
--              Put_Line (Routine_Name & "L289 Binary_Log_Loss" &
--                          Float'Image (Loss));
         when Log_Loss_Function =>
            Loss := Log_Loss (Y_Prob, Activations.Last_Element);
--              Put_Line (Routine_Name & "L289 Log_Loss" & Float'Image (Loss));
         when Squared_Error_Function =>
            Loss := Squared_Loss (Y_Prob, Activations.Last_Element);
--              Put_Line (Routine_Name & "L289 Squared_Loss" & Float'Image (Loss));
      end case;

--        Put_Line (Routine_Name & "L289 Loss" & Float'Image (Loss));
      --  L289  Add L2 regularization term to loss
      --  for s in self.coefs_:
      Sum_Sq_Coeffs := 0.0;
      for layer in Self.Attributes.Params.First_Index ..
        Self.Attributes.Params.Last_Index loop
         declare
            Coeffs : constant Real_Float_Matrix :=
                       Self.Attributes.Params (Layer).Coeff_Gradients;
            --  numpy.ravel (a) returns the elements of a as a 1-D array.
            --              Ravel  : Real_Float_Vector
            --                (1 .. Coeffs'Length * Coeffs'Length (2));
         begin
            for row in Coeffs'Range loop
               for col in Coeffs'Range (2) loop
                  Sum_Sq_Coeffs := Sum_Sq_Coeffs + Coeffs (row, col) ** 2;
                  --                    Ravel ((row - 1) * Coeffs'Length (2) +
                  --                             col - Coeffs'First (2) + 1) :=
                  --                      Coeffs (row, col);
               end loop;
            end loop;
            --              Sum_Sq_Coeffs := Sum_Sq_Coeffs + Ravel * Ravel;
         end;  --  declare
      end loop;

      --  L292
      Loss := Loss + 0.5 * (Self.Parameters.Alpha *
                              Sum_Sq_Coeffs / Float (Num_Samples));
--        Put_Line (Routine_Name & "L292 Loss" & Float'Image (Loss));

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
      Deltas.Set_Length (Count_Type (Self.Attributes.N_Layers - 1));
      --        Printing.Print_Float_Matrix (Routine_Name & "L301+ Activations last",
      --                 Activations.Last_Element, 1, 2);
      --        Printing.Print_Float_Matrix (Routine_Name & "L301+ Y_Float", Y_Float);
      Assert (Activations.Last_Element'Length (2) = Y_Prob'Length (2),
              Routine_Name & "L301 last Activations item width" &
                Integer'Image (Activations.Last_Element'Length (2)) &
                " differs from Y_Prob width" &
                Integer'Image (Y_Prob'Length (2)));
      Deltas.Replace_Element (Deltas.Last_Index,
                              Activations.Last_Element - Y_Prob);

      --  L304  Compute gradient for the last layer
      Compute_Loss_Gradient (Self, Self.Attributes.N_Layers - 1, Num_Samples,
                             Activations, Deltas, Gradients);

      --  L310, L308
      for layer in reverse 2 .. Self.Attributes.N_Layers - 1 loop
         Update_Hidden_Layer_Gradients
           (Self, Activations, Deltas, Gradients, layer, Num_Samples);
      end loop;

      --          for index in Deltas.First_Index .. Deltas.Last_Index loop
      --              Printing.Print_Float_Matrix
      --                (Routine_Name & "Deltas " & Integer'Image (index),
      --                 Deltas (index));
      --          end loop;

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
      if Hidden_Layer_Sizes.Is_Empty then
         --  L1276 Default Hidden_Layer_Sizes
         Classifier.Parameters.Hidden_Layer_Sizes.Append (100);
      else
         Classifier.Parameters.Hidden_Layer_Sizes := Hidden_Layer_Sizes;
      end if;
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
   procedure Compute_Loss_Gradient
     (Self          : MLP_Classifier;
      Layer         : Positive;
      Num_Samples   : Positive;
      Activations   : Real_Matrix_List;
      Deltas        : Real_Matrix_List;
      Gradients     : in out Parameters_List) is
      --        use Ada.Containers;
      use Real_Float_Arrays;
      --        Routine_Name        : constant String :=
      --            "Multilayer_Perceptron.Compute_Loss_Gradient ";
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
      New_Gradients       : Parameters_Record
        (New_Coeff_Gradients'Length, New_Coeff_Gradients'Length (2));
   begin
      New_Coeff_Gradients :=
        (New_Coeff_Gradients + Self.Parameters.Alpha *
           Self.Attributes.Params (layer).Coeff_Gradients) /
          Float (Num_Samples);
      --        Put_Line (Routine_Name & "Deltas length" &
      --  Count_Type'Image (Deltas.Length));
      --        Put_Line (Routine_Name & "New_Coeff_Gradients size" &
      --  Integer'Image (New_Coeff_Gradients'Length) & " x" &
      --  Integer'Image (New_Coeff_Gradients'Length (2)));
      --        Put_Line (Routine_Name & "Activations size" &
      --  Integer'Image (Activations.Element (layer)'Length) &
      --" x" &
      --  Integer'Image (Activations.Element (layer)'Length (2)));
      --        Put_Line (Routine_Name & "Deltas size" &
      --  Integer'Image (Deltas.Element (layer)'Length) & " x" &
      --  Integer'Image (Deltas.Element (layer)'Length (2)));
      --        Put_Line (Routine_Name & "New_Intercept_Grads length" &
      --  Integer'Image (New_Intercept_Grads'Length));
      --        Put_Line (Routine_Name & "New_Gradients.Intercept_Grads length" &
      --  Integer'Image (New_Gradients.Intercept_Grads'Length));
      New_Gradients.Coeff_Gradients := New_Coeff_Gradients;
      New_Gradients.Intercept_Grads := New_Intercept_Grads;
      Gradients.Prepend (New_Gradients);

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
   begin
      Assert (not Hidden_Layer_Sizes.Is_Empty, Routine_Name &
                "Hidden_Layer_Sizes is empty");
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
      end if;

      Activations.Append (X);

      --  L417 Initialized grads are empty vectors, no initialization needed.
      --  L427
      if Self.Parameters.Solver = Sgd_Solver or else
        Self.Parameters.Solver = Adam_Solver then
         Fit_Stochastic (Self, X, Y_Bin, Incremental);

         --  L444
      elsif Self.Parameters.Solver = Lbfgs_Solver then
         Fit_Lbfgs (Self, X, Y_Bin, Activations);
      end if;

      Check_Weights (Self);

   end  Fit;

   --  -------------------------------------------------------------------------
   --  L377  MultilayerPerceptron._Fit
   --  L759  MultilayerPerceptron.Fit
   --  The Y values are class labels in classification,
   --  real numbers in regression.
   procedure Fit (Self  : in out MLP_Classifier;
                  X     : Real_Float_Matrix;
                  Y     : Integer_Matrix; Incremental : Boolean := False) is
      use Ada.Containers;
      Routine_Name       : constant String :=
                             "Multilayer_Perceptron.Fit Integer Y ";
      Num_Features       : constant Positive := Positive (X'Length (2));
      Hidden_Layer_Sizes : constant Integer_List :=
                             Self.Parameters.Hidden_Layer_Sizes;
      --  L394
      Y_Bin              : constant Binary_Matrix :=
                             Validate_Input (Self, Y, Incremental);
      Layer_Units        : Integer_List;
      Activations        : Real_Matrix_List;
   begin
      Assert (not Hidden_Layer_Sizes.Is_Empty, Routine_Name &
                "Hidden_Layer_Sizes is empty");
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
      --        Printing.Print_Integer_Matrix (Routine_Name & "L409 Y", Y, 1, 15);
      if First_Pass then
         Initialize (Self, Layer_Units);
      end if;

      Activations.Append (X);

      --  L417 Initialized grads are empty vectors, no initialization needed.
      --  L427
      if Self.Parameters.Solver = Sgd_Solver or else
        Self.Parameters.Solver = Adam_Solver then
         Fit_Stochastic (Self, X, Y_Bin, Incremental);

         --  L444
      elsif Self.Parameters.Solver = Lbfgs_Solver then
         Fit_Lbfgs (Self, X, Y_Bin, Activations);
      end if;

      Check_Weights (Self);

   end Fit;

   --  -------------------------------------------------------------------------
   --  L516
   procedure Fit_Lbfgs (Self            : in out MLP_Classifier;
                        X               : Real_Float_Matrix;
                        Y               : Binary_Matrix;
                        Activations     : in out Real_Matrix_List) is
      --  Deltas          : in out Real_Matrix_List;
      --  Grads           : in out Parameters_List;
      --  Layer_Units     : Integer_List) is
      Routine_Name : constant String := "Multilayer_Perceptron.Fit_Lbfgs ";
      --        Num_Samples  : constant Positive := Positive (X'Length);
      --        Start        : Positive := 1;
      --        Last         : Positive;
      --        N_Fan_In     : Positive;
      --        N_Fan_Out    : Positive;
      --        Options      : constant Opt_Minimise.Minimise_Options :=
      --     (Self.Parameters.Max_Fun, Self.Parameters.Max_Iter,
      --      Self.Parameters.Tol, 20);
      Grads        : constant Parameters_List := Self.Attributes.Params;
      --  Opt_Result is the optimization result represented as a Optimise_Result
      --  record.
      --  Important attributes are: the solution array X, a Boolean flag
      --  Success indicating if the optimizer exited successfully

      Args         : Loss_Grad_Args (X'Length, X'Length (2), Y'Length (2));
   begin
      Put_Line (Routine_Name & "Grads length: " &
                  Integer'Image (Integer (Grads.Length)));
      Put_Line (Routine_Name &
                  "Grads (1).Coeff_Gradients size" &
                  Integer'Image (Grads (1).Coeff_Gradients'Length) & " x"
                & Integer'Image (Grads (1).Coeff_Gradients'Length (2)));
      Put_Line (Routine_Name &
                  "Grads.Intercept_Grads (1) length" &
                  Integer'Image (Grads (1).Intercept_Grads'Length));
      Put_Line (Routine_Name &
                  "Grads (2).Coeff_Gradients size" &
                  Integer'Image (Grads (2).Coeff_Gradients'Length) & " x"
                & Integer'Image (Grads (2).Coeff_Gradients'Length (2)));
      Put_Line (Routine_Name &
                  "Grads.Intercept_Grads (2) length" &
                  Integer'Image (Grads (2).Intercept_Grads'Length));
      --        --  L524  Save sizes and indices of coefficients for faster unpacking
      --        for index in 1 .. Self.Attributes.N_Layers - 1 loop
      --           N_Fan_In := Layer_Units.Element (index);
      --           N_Fan_Out := Layer_Units.Element (index + 1);
      --           Last := Start + N_Fan_In * N_Fan_Out;
      --           Self.Attributes.Coef_Indptr.Append ((Start, Last,
      --       N_Fan_In, N_Fan_Out));
      --           Start := Last + 1;
      --        end loop;
      --
      --        --  L524  Save sizes and indices of intercepts for faster unpacking
      --          Start := 1;
      --        for index in 1 .. Self.Attributes.N_Layers - 1 loop
      --           Last := Start + N_Fan_In * N_Fan_Out;
      --           Self.Attributes.Intercept_Indptr.Append ((Start, Last));
      --           Start := Last + 1;
      --        end loop;

      Args.Self := Self;
      Args.Params := Grads;
      Args.X := X;
      Args.Y := Y;
      Args.Activations := Activations;
      Args.Gradients := Grads;

      --  L546  Grads is similar to packed_coef_inter
      declare
         Opt_Result : constant Optimise.Optimise_Result
           := Opt_Minimise.Minimise
             (Fun => Loss_Grad_LBFGS'Access, Args => Args, X0 => Grads,
              Method => Opt_Minimise.L_BFGS_B_Method,
              Jac => Num_Diff.FD_True);
      begin
         Self.Attributes.N_Iter :=
           Utils_Optimise.Check_Optimize_Result (Opt_Result, Self.Parameters.Max_Iter);

         --        declare
         --           X_Vec : constant Real_Float_Vector := Result.X;
         --        begin
         --  L566
         Self.Attributes.Loss := Opt_Result.Fun.Loss;
         --        end;
      end;

   end Fit_Lbfgs;

   --  -------------------------------------------------------------------------

   --  L563
   procedure Fit_Stochastic (Self        : in out MLP_Classifier;
                             X           : Real_Float_Matrix;
                             Y           : Binary_Matrix;
                             Incremental : Boolean := False) is
      --        use Estimator;
      Routine_Name     : constant String :=
                           "Multilayer_Perceptron.Fit_Stochastic ";
      --        Is_Classifier  : constant Boolean :=
      --       Self.Estimator_Kind = Classifier_Estimator;
      Num_Samples      : constant Positive := Positive (X'Length);
      --        Num_Features   : constant Positive := Positive (X'Length (2));
      --        Num_Classes    : constant Positive :=
      --       Positive (Self.Attributes.Classes.Length);
      --        LE_U           : Label.Label_Binarizer;
      Iter             : Natural := 0;
      Continue         : Boolean := True;
      Early_Stopping   : Boolean := Self.Parameters.Early_Stopping;
      Batch_Size       : Positive;
      Batches          : Slices_List;
      Accumulated_Loss : Float := 0.0;
      Msg              : Unbounded_String;
      Is_Stopping      : Boolean := False;
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
--        Put_Line (Routine_Name & "Num_Samples" & Integer'Image (Num_Samples));
--        Put_Line (Routine_Name & "Batch_Size" & Integer'Image (Batch_Size));
      Batches := Utils.Gen_Batches (Num_Samples, Batch_Size);

      --  L628
      --        Put_Line (Routine_Name & "Max_Iter" &
      --                    Integer'Image (Self.Parameters.Max_Iter));
      while Continue and then Iter < Self.Parameters.Max_Iter loop
         Iter := Iter + 1;
         --  Shuffling done in Process_Batch
         Accumulated_Loss := 0.0;
         --  L636
         for Batch_index in Batches.First_Index ..
           Batches.Last_Index loop
            --  L649
--              Put_Line (Routine_Name & "L649 Batch_index" &
--                          Integer'Image (Batch_index));
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
            Put_Line (Routine_Name & "Iteration" &
                        Integer'Image (Self.Attributes.N_Iter) &
                        ", loss = " & Float'Image (Self.Attributes.Loss));
         end if;

         --  L669 Update no_improvement_count based on training loss or
         --       validation score according to early_stopping
         Update_No_Improvement_Count (Self);
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
            Put_Line (Routine_Name & "Tol:" &
                        Float'Image (Self.Parameters.Tol));
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

      --  Check that Activations.Last_Element rows are probabilities
      Is_Probilities_Matrix (Routine_Name & "Activations.Last_Element ",
                             Activations.Last_Element);
   end Forward_Pass;

   --  -------------------------------------------------------------------------
   --  L144
   function Forward_Pass_Fast (Self  : MLP_Classifier; X : Real_Float_Matrix)
                               return Real_Float_Matrix is
      --        use Ada.Containers;
      use Base_Neural;
      use Real_Float_Arrays;
      use Parameters_Package;
      type Activations_Array is array (Integer range <>) of Real_Float_List;
      --        Routine_Name : constant String :=
      --                         "Multilayer_Perceptron.Forward_Pass_Fast ";

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
      Params_List       : constant Parameters_List := Self.Attributes.Params;
      Activ_Out         : Real_Float_Matrix
        (X'Range, 1 .. Integer (Self.Attributes.N_Outputs));
      --  Activations_Array used to allow for different sized matrices
      Activations       : Activations_Array (X'Range);
   begin
      --  L160 Initialize first layer
      for row in X'Range loop
         for col in X'Range (2) loop
            Activations (row).Append (X (row, col));
         end loop;
      end loop;

      --  L167 Forward propagate
      --  python range(self.n_layers_ - 1) = 0 .. self.n_layers_ - 1
      for layer in 1 .. Num_Layers - 1 loop
         declare
            Params             : constant Parameters_Record :=
                                   Params_List (layer);
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

      Activ_Out := To_Matrix (Activations);

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
      --  Routine_Name : constant String := "Multilayer_Perceptron.Init_Coeff ";
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
      --  Generate random weights, Random_Float -1.0 .. 1.0
      for f_in in 1 .. Fan_In loop
         for f_out in 1 .. Fan_Out loop
            Params.Coeff_Gradients (f_in, f_out) :=
              Init_Bound * Random_Float;
         end loop;
      end loop;

      --  Generate random bias
      for f_out in 1 .. Fan_Out loop
         Params.Intercept_Grads (f_out) := Init_Bound * Random_Float;
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
      Routine_Name   : constant String := "Multilayer_Perceptron.Initialize ";
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
      Printing.Print_Integer_List (Routine_Name & "Layer_Units", Layer_Units);
      --  L351
      --  python range(self.n_layers_ - 1) => 0 .. self.n_layers_ - 1
      for layer in 1 .. Self.Attributes.N_Layers - 1 loop
         --  Add coefficent matrices and intercept vectors for layer.
         Fan_In := Layer_Units (layer);
         Fan_Out := Layer_Units (layer + 1);
         Self.Attributes.Params.Append (Init_Coeff (Self, Fan_In, Fan_Out));
      end loop;
--        Put_Line
--          (Routine_Name & "layer 2 Coeff_Gradients size" &
--             Integer'Image (Self.Attributes.Params.Element (2).Coeff_Gradients'Length));
--        Printing.Print_Float_Matrix
--          (Routine_Name & "layer 2 Coeff_Gradients",
--           Self.Attributes.Params.Element (2).Coeff_Gradients, 1, 4);

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
      Sum : Real_Float_Vector (PM'Range) := (others => 0.0);
   begin
      for row in PM'Range loop
         for col in PM'Range (2) loop
            Assert (PM (row, col) >= 0.0, Msg & "Matrix" &
                      Integer'Image (row) & "," & Integer'Image (col) & " = " &
                      Float'Image (PM (row, col)));
            Sum (row) := Sum (row) + PM (row, col);
         end loop;
      end loop;

      for row in Sum'Range loop
         Assert (Sum (row) >= 1.0 - 10.0 ** (-6) and
                   Sum (row) <= 1.0 + 10.0 ** (-6), Msg &
                   " (" & Integer'Image (row) & ") Sum = " & Float'Image (Sum (row)));
      end loop;

   end Is_Probilities_Matrix;

   --  -------------------------------------------------------------------------

   function Loss_Grad_LBFGS (Args : Loss_Grad_Args) return Loss_Grad_Result is
      Self        : MLP_Classifier := Args.Self;
      Gradients   : constant Parameters_List := Args.Params;
      Activations : Real_Matrix_List := Args.Activations;
      Loss        : constant Float := Float'Safe_Last;
   begin
      --  L236 _unpack
      Self.Attributes.Params := Args.Params;
      --  L237 _backprop
      Forward_Pass (Self, Activations);
      --          Backprop (Self, Args.X, Args.Y, Activations, Loss, Gradients);

      return (Loss, Gradients);

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
      --  Routine_Name : constant String := "Multilayer_Perceptron.Partial_Fit 2 ";
   begin
      if Check_Partial_Fit_First_Call (Self, Classes) then
         Self.Attributes.Binarizer := LB;
         if Type_Of_Target (Y) = Y_Multilabel_Indicator then
            Fit (Self.Attributes.Binarizer, Y);
         else
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

   procedure Permute (X : in out Real_Float_Matrix;
                      Y : in out Binary_Matrix) is
      Num_Samples : constant Positive := Positive (X'Length);
      Rand        : Positive;
   begin
      if Num_Samples > 1 then
         for index in 1 .. Num_Samples - 1 loop
            Rand := index +
              Natural (abs (Maths.Random_Float) * Float (Num_Samples - index));
            Utilities.Swap (X, index, Rand);
            Utilities.Swap (Y, index, Rand);
         end loop;
      end if;

   end Permute;

   --  -------------------------------------------------------------------------
   --  L1168
   function Predict (Self : MLP_Classifier; X : Real_Float_Matrix)
                     return Binary_Matrix is
      Routine_Name   : constant String := "Multilayer_Perceptron.Predict ";
      Y_Pred         : constant Real_Float_Matrix :=
                         Forward_Pass_Fast (Self, X);
   begin
      Printing.Print_Float_Matrix (Routine_Name & "Y_Pred", Y_Pred, 1, 4);
      Printing.Print_Binary_Matrix
        (Routine_Name & "Inverse_Transform",
         Label.Inverse_Transform (Self.Attributes.Binarizer, Y_Pred), 1, 4);
      return Label.Inverse_Transform (Self.Attributes.Binarizer, Y_Pred);

   end Predict;

   --  -------------------------------------------------------------------------
   --  L637
   procedure Process_Batch (Self             : in out MLP_Classifier;
                            X                : Real_Float_Matrix;
                            Y                : Binary_Matrix;
                            Batch_Slice      : Slice_Record;
                            Batch_Size       : Positive;
                            Accumulated_Loss : in out Float) is
--        Routine_Name   : constant String :=
--                               "Multilayer_Perceptron.Process_Batch ";
      Num_Features   : constant Positive := Positive (X'Length (2));
      Num_Classes    : constant Positive := Y'Length (2);
      --  X_Batch: samples x features
      X_Batch        : Real_Float_Matrix (1 .. Batch_Size, 1 .. Num_Features);
      Y_Batch        : Binary_Matrix (1 .. Batch_Size, 1 .. Num_Classes);
      --  Activations: layers x samples x features
      Activations    : Real_Matrix_List;
      Gradients      : Parameters_List;
      Batch_Row      : Positive;
      Batch_Loss     : Float;
   begin
--        Printing.Print_Binary_Matrix (Routine_Name & "Y", Y);
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
         Permute (X_Batch, Y_Batch);
      end if;

      --  L644  Initialize Activations
      Activations.Clear;
      Activations.Append (X_Batch);

      --  L645
      Forward_Pass (Self, Activations);
      Backprop (Self, X_Batch, Y_Batch, Activations, Batch_Loss, Gradients);

      --  L665
      Accumulated_Loss := Accumulated_Loss + Batch_Loss *
        Float (Batch_Slice.Last - Batch_Slice.First + 1);
      --          Put_Line (Routine_Name & "Accumulated_Loss" &
      --                      Float'Image (Accumulated_Loss));
      --  L667 update weights
      --  Update_Params updates parameters with given gradients
      Stochastic_Optimizers.Update_Params
        (Self.Attributes.Optimizer, Self.Attributes.Params, Gradients);

   end Process_Batch;

   --  -------------------------------------------------------------------------

   procedure Update_Hidden_Layer_Gradients
     (Self               : in out MLP_Classifier;
      Activations        : Real_Matrix_List;
      Deltas             : in out Real_Matrix_List;
      Gradients          : in out Parameters_List;
      Layer, Num_Samples : Positive) is
      use Base_Neural;
      use Real_Float_Arrays;
      use Real_Matrix_List_Package;
      --  Routine_Name : constant String :=
      --                   "Multilayer_Perceptron.Update_Hidden_Layer_Gradients ";
      Params       : constant Parameters_Record :=
                       Self.Attributes.Params (Layer);
   begin
      --  L311
      Deltas.Replace_Element (Layer - 1, Deltas.Element (Layer) *
                                Transpose (Params.Coeff_Gradients));
      --  Put_Line (Routine_Name & "Activations size" &
      --  Integer'Image (Activations.Element (layer)'Length) &
      --  " x" &
      --  Integer'Image (Activations.Element (layer)'Length (2)));
      --  Put_Line (Routine_Name & "Deltas size" &
      --  Integer'Image (Deltas.Element (layer - 1)'Length) & " x" &
      --  Integer'Image (Deltas.Element (layer - 1)'Length (2)));
      --  Put_Line (Routine_Name & "L312 Activation_Type " &
      --              Activation_Type'Image (Self.Parameters.Activation));
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
         Activations => Activations, Deltas => Deltas, Gradients => Gradients);

   end Update_Hidden_Layer_Gradients;

   --  -------------------------------------------------------------------------
   --  L716 Early_Stopping
   --     procedure Update_No_Improvement_Count
   --       (Self   : in out MLP_Classifier; Early_Stopping : Boolean;
   --        X_Val  : Real_Float_Matrix; Y_Val : Integer_Matrix) is
   --        Routine_Name     : constant String
   --          := "Multilayer_Perceptron.Update_No_Improvement_Count ";
   --        Sample_Weight    : constant Real_Float_Vector (1 .. 0) := (others => 0.0);
   --        Last_Valid_Score : Float;
   --        Score_Val        : Float;
   --     begin
   --        if Early_Stopping then
   --           Score_Val := Base.Score (Self, X_Val, Y_Val, Sample_Weight);
   --           Self.Parameters.Validation_Scores.Append (Score_Val);
   --           Last_Valid_Score := Self.Parameters.Validation_Scores.Last_Element;
   --           if Self.Parameters.Verbose then
   --              Put_Line (Routine_Name & "Validation score: " &
   --      Float'Image (Last_Valid_Score));
   --           end if;
   --
   --           --  L728
   --           if Last_Valid_Score <
   --             Self.Parameters.Best_Validation_Score + Self.Parameters.Tol then
   --              Self.Attributes.No_Improvement_Count :=
   --                Self.Attributes.No_Improvement_Count + 1;
   --           else
   --              Self.Attributes.No_Improvement_Count := 0;
   --           end if;
   --
   --           if Last_Valid_Score > Self.Parameters.Best_Validation_Score then
   --              Self.Parameters.Best_Validation_Score := Last_Valid_Score;
   --              Self.Parameters.Best_Params := Self.Attributes.Params;
   --           end if;
   --
   --        else
   --           if Self.Attributes.Loss_Curve.Last_Element >
   --             Self.Attributes.Best_Loss then
   --              Self.Attributes.No_Improvement_Count :=
   --                Self.Attributes.No_Improvement_Count + 1;
   --           else
   --              Self.Attributes.No_Improvement_Count := 0;
   --           end if;
   --
   --           if Self.Attributes.Loss_Curve.Last_Element <
   --             Self.Attributes.Best_Loss then
   --              Self.Attributes.Best_Loss :=
   --                Self.Attributes.Loss_Curve.Last_Element;
   --           end if;
   --        end if;
   --
   --     end Update_No_Improvement_Count;

   --  -------------------------------------------------------------------------
   --  L716 not Early_Stopping
   procedure Update_No_Improvement_Count
     (Self   : in out MLP_Classifier) is
      --        Routine_Name     : constant String
      --          := "Multilayer_Perceptron.Update_No_Improvement_Count ";
   begin
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
         --           Put_Line (Routine_Name & "L1139 classes empty: " &
         --                    Boolean'Image (Self.Attributes.Classes.Is_Empty));
         Self.Attributes.Binarizer := Binarizer;
         Label.Fit (Self.Attributes.Binarizer, Y);
         Self.Attributes.Classes := Self.Attributes.Binarizer.Classes;
      else
         --  L1143
         --           Put_Line (Routine_Name & "L1143");
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
      return Label.Transform (Self.Attributes.Binarizer, Y);

   end Validate_Input;

   --  -------------------------------------------------------------------------

end Multilayer_Perceptron;
