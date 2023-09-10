
with Ada.Text_IO; use Ada.Text_IO;

with Maths;

with Base_Neural;
with Basic_Printing; use Basic_Printing;
with Stochastic_Optimizers;

package body Structure_V2 is

--     procedure Forward (aModel      : in out Sequential_Model;
--                        Loss_Method : Loss_Kind);

   --  -------------------------------------------------------------------------

   procedure Add_Connections (aModel : in out Sequential_Model;
                              Prev_Layer : Layer; thisLayer : in out Layer) is
      Connect : Connection (Prev_Layer.Nodes'Length,
                                   thisLayer.Nodes'Length);
   begin
      for row in Connect.Connection_Matrix'Range loop
         for col in Connect.Connection_Matrix'Range (2) loop
         --  Random_Float generates a random number in the range  -1.0 .. 1.0
         Connect.Connection_Matrix (row, col) := Maths.Random_Float;
         end loop;
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
      Prev_Layer : constant Layer := aModel.Layers.Last_Element;
      Prev_Nodes : constant Real_Float_Vector := Prev_Layer.Nodes;
      thisLayer  : Layer (Prev_Nodes'Length);
   begin
      Add_Connections (aModel, Prev_Layer, thisLayer);
      thisLayer.Nodes := aModel.Connections.Last_Element.Connection_Matrix *
        Prev_Layer.Nodes;
      aModel.Layers.Append (thisLayer);

   end Add_Layer;

   --  ---------------------------------------------------------------------------

--     procedure Add_Node (aLayer : in out Layer; Features : Real_Float_Vector) is
--        aNode : Node (1 .. Features'Length);
--     begin
--        aNode.Features := Features;
--        --  Initialize weights
--        for index in aNode.Weights'Range loop
--           --  Random_Float generates a random number in the range  -1.0 .. 1.0
--           aNode.Weights (index) := Maths.Random_Float;
--        end loop;
--        aLayer.Nodes.Append (aNode);
--
--     end Add_Node;
--
   --  ---------------------------------------------------------------------------

   procedure Back_Propogate (aModel      : Sequential_Model;
                             Loss_Method : Loss_Kind) is
   begin
      null;

   end Back_Propogate;

   --  -------------------------------------------------------------------------

   procedure Compile (aModel      : in out Sequential_Model;
                      Loss_Method : Loss_Kind) is
   begin
      null;
--        Forward (aModel, Loss_Method);

   end Compile;

   --  -------------------------------------------------------------------------

--     procedure Forward (aModel      : in out Sequential_Model;
--                        Loss_Method : Loss_Kind) is
--        use Real_Float_Arrays;
--        use Base_Neural;
--        use Layer_Packge;
--  --        use Nodes_Package;
--        Routine_Name : constant String := "Structure.Forward ";
--        Out_Value    : Float;
--     begin
--        Put_Line (Routine_Name & "Num layers:" &
--                    Integer'Image (Integer (aModel.Layers.Length)));
--        --  Update first layer
--        for node_id in aModel.Layers.First_Element.Nodes.First_Index ..
--          aModel.Layers.First_Element.Nodes.Last_Index loop
--
--           Out_Value :=
--             aModel.Layers.First_Element.Nodes (node_id).Weights *
--             aModel.Layers.First_Element.Nodes (node_id).Features +
--             aModel.Layers.First_Element.Nodes (node_id).Bias;
--
--           case aModel.Layers.First_Element.Activation is
--              when Identity_Activation => null;
--              when ReLu_Activation => Rect_LU (Out_Value);
--              when Sigmoid_Activation =>
--                 Out_Value := Sigmoid (Out_Value);
--              when Soft_Max_Activation => Softmax (Out_Value);
--           end case;
--
--           aModel.Layers (aModel.Layers.First_Index).Output_Data (node_id) :=
--             Out_Value;
--           case Loss_Method is
--              when Mean_Square_Error_Loss => null;
--           end case;
--
--           aModel.Layers (aModel.Layers.First_Index).Loss (node_id) :=
--             Out_Value - aModel.Labels (1);
--
--           Put_Line (Routine_Name & "Layer, Node" & Integer'Image (aModel.Layers.First_Index) &
--                       "," & Integer'Image (node_id) & " Out_Value: " &
--                       Float'Image (aModel.Layers (aModel.Layers.First_Index).Output_Data
--                       (node_id)));
--        end loop;  --  first layer nodes updatd
--
--        --  Update other layers
--        for layer in aModel.Layers.First_Index + 1 ..
--          aModel.Layers.Last_Index loop
--           Put_Line (Routine_Name & "Layer:" & Integer'Image (layer));
--
--           for node_id in aModel.Layers (layer).Nodes.First_Index ..
--             aModel.Layers (layer).Nodes.Last_Index loop
--              aModel.Layers (layer).Nodes (node_id).Features :=
--                aModel.Layers (layer - 1).Output_Data;
--
--              Out_Value :=
--                aModel.Layers (layer).Nodes (node_id).Weights *
--                aModel.Layers (layer).Nodes (node_id).Features +
--                aModel.Layers (layer).Nodes (node_id).Bias;
--
--              case aModel.Layers (layer).Activation is
--                 when Identity_Activation => null;
--                 when ReLu_Activation => Rect_LU (Out_Value);
--                 when Sigmoid_Activation => Out_Value := Sigmoid (Out_Value);
--                 when Soft_Max_Activation => Softmax (Out_Value);
--              end case;
--
--              aModel.Layers (layer).Output_Data (node_id) :=
--                Out_Value;
--              aModel.Layers (layer).Loss (node_id) :=
--                Out_Value - aModel.Labels (1);
--              Put_Line (Routine_Name & "Layer, Node" & Integer'Image (layer) &
--                          "," & Integer'Image (node_id) & " Out_Value: " &
--                          Float'Image (aModel.Layers (layer).Output_Data
--                          (node_id)));
--           end loop;  --  node updated
--
--        end loop;  --  other layer nodes
--
--     end Forward;

   --  ---------------------------------------------------------------------------

--     function Get_Output_Value (aModel : Sequential_Model) return Real_Float_Vector is
--     begin
--        return aModel.Layers.Last_Element.Output_Data;
--
--     end Get_Output_Value;

   --  ---------------------------------------------------------------------------

end Structure_V2;
