
with Ada.Assertions;
with Ada.Text_IO; use Ada.Text_IO;

with Maths;

with Base_Neural;
with Basic_Printing; use Basic_Printing;
with Neural_Maths;
with Stochastic_Optimizers;

package body Neural_Model is

   procedure Forward (aModel : in out Sequential_Model);

   --  -------------------------------------------------------------------------

   procedure Add_Connections (aModel     : in out Sequential_Model;
                              Prev_Layer : Layer; thisLayer : in out Layer) is
      use Stochastic_Optimizers;
      --        Routine_Name : constant String := "Neural_Model.Add_Connections ";
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
            Connect.Coeff_Gradients (row, col) :=
              0.5 * abs (Maths.Random_Float);
         end loop;
         Connect.Intercept_Grads (row)  := 0.5 * abs (Maths.Random_Float);
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
      --        Routine_Name : constant String := "Neural_Model.Add_Layer others  ";
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

   function Back_Propogate (aModel    : in out Sequential_Model;
                            Optimiser : Stochastic_Optimizers.Optimizer_Record;
                            Loss      : Real_Float_Vector)
                            return Stochastic_Optimizers.Parameters_List is
      use Stochastic_Optimizers;
      use Real_Float_Arrays;
      use Real_Matrix_List_Package;
      Routine_Name        : constant String :=
                              "Neural_Model.Back_Propogate ";
      --        Pred_Params (Self      : in out Optimizer_Record;
      --                       Params    : in out Parameters_List;
      --                       Gradients : Parameters_List);
      Deltas              : Real_Matrix_List;
      Sum_Sq_Coeffs       : Float := 0.0;
      Pred_Gradients      : Parameters_List;
   begin
      --    Pred_Params (Optimiser, aModel.Connections, Gradients);

      for layer_index in reverse
        aModel.Layers.First_Index .. aModel.Layers.Last_Index loop
         null;
      end loop;

      return Pred_Gradients;

   end Back_Propogate;

   --  -------------------------------------------------------------------------

   function Backward (aModel : in out Sequential_Model;
                      Loss   : Real_Float_Vector)
                      return Real_Float_Vector is
      use Real_Float_Arrays;
      Routine_Name        : constant String :=
                              "Neural_Model.Backward ";
      Input_Error         : constant Real_Float_Vector
        := Loss * Transpose (aModel.Connections.Last_Element.Coeff_Gradients);
      Weights_Error       : constant Real_Float_Vector
        := Transpose (aModel.Connections.First_Element.Coeff_Gradients) * Loss;
   begin
      aModel.Delta_Weights := aModel.Delta_Weights + Input_Error;
      aModel.Delta_Bias := aModel.Delta_Bias + Loss;

      return Input_Error;

   end Backward;

   --  -------------------------------------------------------------------------

   function Backward_Activation (aModel : in out Sequential_Model;
                      Loss   : Real_Float_Vector)
                      return Real_Float_Vector is
      use Real_Float_Arrays;
--        Routine_Name        : constant String :=
--                                "Neural_Model.Backward_Activation ";
   begin
      --  From Multilayer_Perceptron.Update_Hidden_Layer_Gradients:
      --  Rect_LU_Derivative (Activations (Layer), Deltas (Layer - 1));
      return Neural_Maths.Sigmoid_Deriv (Loss);

   end Backward_Activation;

   --  -------------------------------------------------------------------------

   procedure Compile (aModel : in out Sequential_Model) is
      use Stochastic_Optimizers;
      Routine_Name : constant String := "Neural_Model.Compile ";
      Pred         : Real_Float_Matrix (1 .. 1, 1 .. aModel.Labels'Length);
      Actual       : Real_Float_Matrix (1 .. 1, 1 .. aModel.Labels'Length);
      Loss         : Real_Float_Vector (1 .. 1);
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
         Loss (1):= Base_Neural.Squared_Loss (Pred, Actual);
      end case;
      Put_Line (Routine_Name & "Loss " & Float'Image (Loss (1)));

      C_Init (Optimiser.Adam, aModel.Connections);
      Gradients := Back_Propogate (aModel, Optimiser, Loss);

   end Compile;

   --  -------------------------------------------------------------------------

   procedure Forward (aModel : in out Sequential_Model) is
      use Real_Float_Arrays;
      use Base_Neural;
      use Stochastic_Optimizers;
      use Layer_Packge;
      Routine_Name : constant String := "Neural_Model.Forward ";
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

end Neural_Model;
