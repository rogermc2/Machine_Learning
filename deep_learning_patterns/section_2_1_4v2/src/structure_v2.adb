
with Ada.Assertions;
with Ada.Text_IO; use Ada.Text_IO;

with Maths;

with Base_Neural;
with Basic_Printing; use Basic_Printing;
with Stochastic_Optimizers;

package body Structure_V2 is

   procedure Forward (aModel : in out Sequential_Model);

   --  -------------------------------------------------------------------------

   procedure Add_Connections (aModel     : in out Sequential_Model;
                              Prev_Layer : Layer; thisLayer : in out Layer) is
      use Stochastic_Optimizers;
      --        Routine_Name : constant String := "Structure.Add_Connections ";
      Connect : Parameters_Record (thisLayer.Nodes'Length,
                                   Prev_Layer.Nodes'Length);
   begin
      --        Put_Line (Routine_Name & "Prev_Layer.Nodes'Length" &
      --                    Integer'Image (Integer (Prev_Layer.Nodes'Length)));
      --        Put_Line (Routine_Name & "thisLayer.Nodes'Length" &
      --                    Integer'Image (Integer (thisLayer.Nodes'Length)));
      for row in Connect.Coeff_Gradients'Range loop
         for col in Connect.Coeff_Gradients'Range (2) loop
            --  Random_Float generates a random number in the range  -1.0 .. 1.0
            Connect.Coeff_Gradients (row, col) := Maths.Random_Float;
         end loop;
         aModel.Activations.Append (Identity_Activation);
      end loop;

      aModel.Connections.Append (Connect);

   end Add_Connections;

   --  ---------------------------------------------------------------------------

   --  Add first layer
   procedure Add_Layer (aModel     : in out Sequential_Model;
                        Num_Nodes  : Positive;
                        Input_Data : Real_Float_Vector) is
      thisLayer : Layer (Num_Nodes);
   begin
      thisLayer.Nodes := Input_Data;
      aModel.Layers.Append (thisLayer);

   end Add_Layer;

   --  ---------------------------------------------------------------------------
   --  Add other layers
   procedure Add_Layer (aModel     : in out Sequential_Model;
                        Num_Nodes  : Positive) is
      use Real_Float_Arrays;
      --        Routine_Name : constant String := "Structure.Add_Layer others  ";
      Prev_Layer : constant Layer := aModel.Layers.Last_Element;
      Prev_Nodes : constant Real_Float_Vector := Prev_Layer.Nodes;
      thisLayer  : Layer (Num_Nodes);
   begin
      --        Put_Line (Routine_Name);
      --        Put_Line (Routine_Name & "Prev_Layer length"&
      --                    Integer'Image (Integer (aModel.Layers.Length)));
      Add_Connections (aModel, Prev_Layer, thisLayer);
      --        Put_Line ( Routine_Name & "Connections added");
      --        Put_Line (Routine_Name & "Prev_Layer Nodes length"&
      --                    Integer'Image (Prev_Layer.Nodes'Length));
      --        Put_Line (Routine_Name & "thisLayer Nodes length" &
      --                    Integer'Image (thisLayer.Nodes'Length));
      --        Print_Matrix_Dimensions (Routine_Name &
      --                                   "Connections.Last_Element.Coeff_Gradients",
      --                                 aModel.Connections.Last_Element.Coeff_Gradients);
      thisLayer.Nodes := aModel.Connections.Last_Element.Coeff_Gradients *
        Prev_Layer.Nodes;
      aModel.Layers.Append (thisLayer);

      --        Put_Line (Routine_Name & "done");
   end Add_Layer;

   --  -------------------------------------------------------------------------
   --  Based on scikit-learn/sklearn/neural_network/_multilayer_perceptron.py
   --  L241
   --  Back_Propogate computes the loss function and its derivatives
   --  with respect to each parameter: weights and bias vectors.
   function Back_Propogate (aModel      : Sequential_Model;
                             Optimiser  : Stochastic_Optimizers.Optimizer_Record;
                            Loss_Method : Loss_Kind)
                            return Stochastic_Optimizers.Parameters_List is
      use Ada.Assertions;
      use Ada.Containers;
      use Base_Neural;
      --        use Parameters_Package;
      use Stochastic_Optimizers;
      use Real_Float_Arrays;
      use Real_Matrix_List_Package;
      Routine_Name       : constant String :=
                             "Structure_V2.Back_Propogate ";
      Pred               : constant Real_Float_Matrix :=
                             To_Real_Float_Matrix (aModel.Labels);
      Loss_Function_Name : Loss_Kind;
      Loss               : Float;
      Deltas             : Real_Matrix_List;
      Sum_Sq_Coeffs      : Float := 0.0;
      Predd_Gradients  : Parameters_List;
   begin
      --  L284
--        Assert (Pred'Length = Integer (aModel.Activations.Last_Element.Length),
--                Routine_Name & "L284 unequal Pred and Activations lengths");
--        Assert (Pred'Length (2) = aModel.Activations.Last_Element'Length (2),
--                Routine_Name & "L284 Pred has a different number of columns" &
--                  Integer'Image (Pred'Length (2)) & " to" &
--                  " Activations.Last_Element columns" &
--                  Integer'Image (Activations.Last_Element'Length (2)));

      if aModel.Loss_Method = Loss_Log and then
        aModel.Activations.Last_Element = Logistic_Activation then
         Loss_Function_Name := Loss_Binary_Log;
      else
         Loss_Function_Name := aModel.Loss_Method;
      end if;

--        case Loss_Function_Name is
--           when Loss_Binary_Log =>
--              Loss := Binary_Log_Loss (Pred, aModel.Activations.Last_Element);
--           when Loss_Log =>
--              Loss := Log_Loss (Pred,  aModel.Activations.Last_Element);
--           when Loss_Mean_Square_Error =>
--              Loss := Squared_Loss (Pred,  aModel.Activations.Last_Element);
--        end case;

      --  L292  Add L2 regularization term to loss
      --  for s in self.coefs_:
      for layer in aModel.Connections.First_Index ..
        aModel.Connections.Last_Index loop
         declare
            Coeffs : constant Real_Float_Matrix :=
                       aModel.Connections (Layer).Coeff_Gradients;
         begin
            for row in Coeffs'Range loop
               for col in Coeffs'Range (2) loop
                  Sum_Sq_Coeffs := Sum_Sq_Coeffs + Coeffs (row, col) ** 2;
               end loop;
            end loop;
         end;  --  declare
      end loop;

      --  L292
--        Loss := Loss + 0.5 * (MLP.Parameters.Alpha *
--                                Sum_Sq_Coeffs / Float (aModel.Num_Samples));

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
      Deltas.Set_Length (Count_Type (aModel.Layers.Length - 1));
--        Assert (Activations.Last_Element'Length (2) = Pred'Length (2),
--                Routine_Name & "L301 last Activations item width" &
--                  Integer'Image (Activations.Last_Element'Length (2)) &
--                  " differs from Pred width" &
--                  Integer'Image (Pred'Length (2)));
      --  L301  310
      Deltas.Replace_Element (Deltas.Last_Index,
                              Activations.Last_Element - Pred);
      Predd_Gradients.Set_Length (Count_Type (aModel.Layers.Length));

      --  L304 Compute gradient for the last layer
      declare
         Grad : constant Parameters_Record :=
                  Compute_Loss_Gradient (MLP, MLP.Attributes.N_Layers - 1,
                                         Num_Samples, Activations, Deltas);
      begin
         Predd_Gradients.Replace_Element
           (Predd_Gradients.Last_Index, Grad);
      end;

      --  L310, L308
      Pred_Hidden_Layer_Gradients
        (MLP, Activations, Deltas, Predd_Gradients, Num_Samples);

      return Predd_Gradients;

   end Back_Propogate;

   --  -------------------------------------------------------------------------

   procedure Compile (aModel : in out Sequential_Model) is
      use Stochastic_Optimizers;
      Routine_Name : constant String := "Structure.Compile ";
      Pred         : Real_Float_Matrix (1 .. 1, 1 .. aModel.Labels'Length);
      Actual       : Real_Float_Matrix (1 .. 1, 1 .. aModel.Labels'Length);
      Loss         : Float;
      Optimiser    : Optimizer_Record (Optimizer_Adam);
      Params       : Parameters_List;  --  list of Parameters_Record
      Gradients    : Parameters_List;
      --        Parameters_Record (Num_Rows, Num_Cols : Positive) is record
      --        Coeff_Gradients : Real_Float_Matrix (1 .. Num_Rows, 1 .. Num_Cols) :=
      --                            (others => (others => 0.0));
      --        Intercept_Grads : Real_Float_Vector (1 .. Num_Cols) := (others => 0.0);
      --  Coefs is a 3D list of weight matrices where the weight matrix at index i
      --  represents the weights between layer i and layer i + 1.
      --  Intercepts is a 2D list of bias vectors where the vector at index
      --  the bias values added to layer i + 1.
   begin
      Forward (aModel);

      for row in aModel.Labels'Range loop
         for col in aModel.Labels'Range loop
            Pred (row, col) := aModel.Labels (col);
            Actual (row, col) := aModel.Layers.Last_Element.Nodes (col);
         end loop;
      end loop;

      case aModel.Loss_Method is
      when Loss_Binary_Log =>
         Put_Line (Routine_Name & "Binary_Log_Loss method not implemented");
      when Loss_Log =>
         Put_Line (Routine_Name & "Log_Loss method not implemented");
      when Loss_Mean_Square_Error =>
         Loss := Base_Neural.Squared_Loss (Pred, Actual);
      end case;
      Put_Line (Routine_Name & "Loss " & Float'Image (Loss));

      C_Init (Optimiser.Adam, aModel.Connections);
--        Pred_Params (Self      : in out Optimizer_Record;
--                       Params    : in out Parameters_List;
--                       Gradients : Parameters_List);
      Gradients := Back_Propogate (aModel, Optimiser, Mean_Square_Error_Loss);
      Pred_Params (Optimiser, aModel.Connections, Gradients);

   end Compile;

   --  -------------------------------------------------------------------------

   procedure Forward (aModel : in out Sequential_Model) is
      use Real_Float_Arrays;
      use Base_Neural;
      use Stochastic_Optimizers;
      use Layer_Packge;
      Routine_Name : constant String := "Structure.Forward ";
   begin
      Put_Line (Routine_Name & "Num layers:" &
                  Integer'Image (Integer (aModel.Layers.Length)));

      for layer in aModel.Layers.First_Index + 1 ..
        aModel.Layers.Last_Index loop
         declare
            Connect : constant Parameters_Record :=
                        aModel.Connections (layer - 1);
         begin
            aModel.Layers (layer).Nodes := Connect.Coeff_Gradients *
              aModel.Layers (layer - 1).Nodes;
            aModel.Layers (layer).Nodes :=
              aModel.Layers (layer).Nodes + Connect.Intercept_Grads;

            case aModel.Activations (layer) is
            when Identity_Activation => null;
            when Logistic_Activation =>
               Put_Line (Routine_Name & "Logistic_Activation not implemented");
            when ReLu_Activation => Rect_LU (aModel.Layers (layer).Nodes);
            when Sigmoid_Activation =>
               Put_Line (Routine_Name & "Sigmoid_Activation not implemented");
            when Soft_Max_Activation => Softmax (aModel.Layers (layer).Nodes);
            end case;

            Print_Float_Vector (Routine_Name & "Layer" &
                                  Integer'Image (layer) & " nodes",
                                aModel.Layers (layer).Nodes);
         end;  --  declare block
      end loop;

   end Forward;

   --  ---------------------------------------------------------------------------

   --     function Get_Output_Value (aModel : Sequential_Model) return Real_Float_Vector is
   --     begin
   --        return aModel.Layers.Last_Element.Output_Data;
   --
   --     end Get_Output_Value;

   --  ---------------------------------------------------------------------------

end Structure_V2;
