
with Ada.Text_IO; use Ada.Text_IO;

with Maths;

with Base_Neural;
with Stochastic_Optimizers;

package body Structure is

   procedure Forward (aModel      : in out Sequential_Model;
                      Loss_Method : Loss_Kind);

   --  -------------------------------------------------------------------------
   --  Add first layer
   procedure Add_Layer (aModel     : in out Sequential_Model;
                        Num_Nodes  : Positive;
                        Input_Data : Real_Float_Vector;
                        Activation : Activation_Kind := Identity_Activation) is
      thisLayer : Layer (Input_Data'Length);
   begin
      for index in 1 .. Num_Nodes loop
         Add_Node (thisLayer, Input_Data);
      end loop;

      thisLayer.Activation := Activation;
      aModel.Layers.Append (thisLayer);

   end Add_Layer;

   --  ---------------------------------------------------------------------------
   --  Add other layers
   procedure Add_Layer (aModel     : in out Sequential_Model;
                        Num_Nodes  : Positive;
                        Activation : Activation_Kind := Identity_Activation) is
      Prev_Layer : constant Layer := aModel.Layers.Last_Element;
      Prev_Nodes : constant Node_List := Prev_Layer.Nodes;
      Input_Data : Real_Float_Vector (1 .. Positive (Prev_Nodes.Length));
      thisLayer  : Layer (Positive (Prev_Nodes.Length));
   begin
      for index in Prev_Nodes.First_Index .. Prev_Nodes.Last_Index loop
         Input_Data (index) := Prev_Layer.Nodes (index).Out_Value;
      end loop;

      --  Initialize weights
      for index in thisLayer.Weights'Range loop
         --  Random_Float generates a random number in the range  -1.0 .. 1.0
         thisLayer.Weights (index) := Maths.Random_Float;
      end loop;

      thisLayer.Activation := Activation;

      for index in 1 .. Num_Nodes loop
         Add_Node (thisLayer, Input_Data);
      end loop;

      aModel.Layers.Append (thisLayer);

   end Add_Layer;

   --  ---------------------------------------------------------------------------

   procedure Add_Node (aLayer : in out Layer; Features : Real_Float_Vector) is
      aNode : Node (Features'Length);
   begin
      aNode.Features := Features;
      aLayer.Nodes.Append (aNode);

   end Add_Node;

   --  ---------------------------------------------------------------------------

   procedure Back_Propogate (aModel : Sequential_Model; Loss_Method : Loss_Kind) is
   begin
      null;

   end Back_Propogate;

   --  ---------------------------------------------------------------------------

   procedure Compile (aModel : in out Sequential_Model; Loss_Method : Loss_Kind) is
   begin
      Forward (aModel, Loss_Method);

   end Compile;

   --  ---------------------------------------------------------------------------

   procedure Forward (aModel      : in out Sequential_Model;
                      Loss_Method : Loss_Kind) is
      use Real_Float_Arrays;
      use Layer_Packge;
      use Nodes_Package;
      Routine_Name : constant String := "Structure.Forward ";
   begin
      Put_Line (Routine_Name & "Num layers:" &
                  Integer'Image (Integer (aModel.Layers.Length)));
      for index in aModel.Layers.First_Index ..
        aModel.Layers.Last_Index - 1 loop
         for index_2 in aModel.Layers (index).Nodes.First_Index ..
           aModel.Layers (index).Nodes.Last_Index - 1 loop
            aModel.Layers (index).Nodes (index_2).Out_Value :=
              aModel.Layers (index).Weights * aModel.Layers (index).Nodes (index_2).Features +
              aModel.Layers (index).Bias;

            case aModel.Layers (index).Activation is
               when Identity_Activation => null;
               when ReLu_Activation =>
                  Base_Neural.Rect_LU
                    (aModel.Layers (index).Nodes (index_2).Out_Value);
               when Sigmoid_Activation =>
                  aModel.Layers (index).Nodes
                    (index_2).Out_Value := Base_Neural.Sigmoid
                    (aModel.Layers (index).Nodes (index_2).Out_Value);
               when Soft_Max_Activation => null;
            end case;
            Put_Line (Routine_Name & "Layer, Node" & Integer'Image (index) &
                        "," & Integer'Image (index_2) & " Out_Value: " &
                        Float'Image (aModel.Layers (index).Nodes
                        (index_2).Out_Value));
         end loop;
      end loop;

      for index_2 in aModel.Layers.Last_Element.Nodes.First_Index ..
        aModel.Layers.Last_Element.Nodes.Last_Index loop
         declare
            aNode : Node := aModel.Layers.Last_Element.Nodes (index_2);
         begin
            aNode.Out_Value :=
              aModel.Layers.Last_Element.Weights * aNode.Features +
                aModel.Layers.Last_Element.Bias;
            case aModel.Layers.Last_Element.Activation is
            when Identity_Activation => null;
            when ReLu_Activation =>
               Base_Neural.Rect_LU (aNode.Out_Value);
            when Sigmoid_Activation =>
               aNode.Out_Value := Base_Neural.Sigmoid (aNode.Out_Value);
            when Soft_Max_Activation => null;
            end case;
            aModel.Layers (aModel.Layers.Last_Index).Nodes (index_2) := aNode;
         end;
         Put_Line (Routine_Name & "Layer, Node" &
                     Integer'Image (aModel.Layers.Last_Index) &
                     "," & Integer'Image (index_2) & " Out_Value: " &
                     Float'Image (aModel.Layers (aModel.Layers.Last_Index).Nodes
                     (index_2).Out_Value));
      end loop;

   end Forward;

   --  ---------------------------------------------------------------------------

   function Get_Output_Value (aModel : Sequential_Model) return Float is
   begin
      return aModel.Layers.Last_Element.Nodes.Last_Element.Out_Value;

   end Get_Output_Value;

   --  ---------------------------------------------------------------------------

end Structure;
