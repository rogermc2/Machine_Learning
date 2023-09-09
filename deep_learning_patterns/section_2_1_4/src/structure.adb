
with Ada.Text_IO; use Ada.Text_IO;

with Maths;

with Base_Neural;
with Basic_Printing; use Basic_Printing;
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
      thisLayer : Layer (Num_Nodes, Input_Data'Length);
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
      thisLayer  : Layer (Num_Nodes, Positive (Prev_Nodes.Length));
   begin
      thisLayer.Activation := Activation;

      for index in 1 .. Num_Nodes loop
         Add_Node (thisLayer, Prev_Layer.Output_Data);
      end loop;

      aModel.Layers.Append (thisLayer);

   end Add_Layer;

   --  ---------------------------------------------------------------------------

   procedure Add_Node (aLayer : in out Layer; Features : Real_Float_Vector) is
      aNode : Node (Features'Length);
   begin
      aNode.Features := Features;
      --  Initialize weights
      for index in aNode.Weights'Range loop
         --  Random_Float generates a random number in the range  -1.0 .. 1.0
         aNode.Weights (index) := Maths.Random_Float;
      end loop;
      aLayer.Nodes.Append (aNode);

   end Add_Node;

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
      Forward (aModel, Loss_Method);

   end Compile;

   --  -------------------------------------------------------------------------

   procedure Forward (aModel      : in out Sequential_Model;
                      Loss_Method : Loss_Kind) is
      use Real_Float_Arrays;
      use Layer_Packge;
      use Nodes_Package;
      Routine_Name : constant String := "Structure.Forward ";
   begin
      Put_Line (Routine_Name & "Num layers:" &
                  Integer'Image (Integer (aModel.Layers.Length)));
      --  Update first layer
      for node_id in aModel.Layers.First_Element.Nodes.First_Index ..
        aModel.Layers.First_Element.Nodes.Last_Index loop
         --           Print_Float_Vector (Routine_Name & "Layer, Node " &
         --                                 Integer'Image (aModel.Layers.First_Index) &
         --                                 "," & Integer'Image (node_id) & "Features",
         --                               aModel.Layers.First_Element.Nodes (node_id).Features);

         aModel.Layers (aModel.Layers.First_Index).Output_Data (node_id) :=
           aModel.Layers.First_Element.Nodes (node_id).Weights *
             aModel.Layers.First_Element.Nodes (node_id).Features +
           aModel.Layers.First_Element.Nodes (node_id).Bias;

--           Print_Float_Vector (Routine_Name & "Layer, Node" &
--                                 Integer'Image (aModel.Layers.First_Index) &
--                                 "," & Integer'Image (node_id) & " Weights",
--                               aModel.Layers.First_Element.Nodes (node_id).Weights);
--           Put_Line (Routine_Name & "Layer, Node" &
--                       Integer'Image (aModel.Layers.First_Index) &
--                       "," & Integer'Image (node_id) & " Out_Value: " &
--                       Float'Image (aModel.Layers
--                       (aModel.Layers.First_Index).Output_Data (node_id)));
         case aModel.Layers.First_Element.Activation is
            when Identity_Activation => null;
            when ReLu_Activation =>
               Base_Neural.Rect_LU
                 (aModel.Layers (aModel.Layers.First_Index).Output_Data (node_id));
            when Sigmoid_Activation =>
               aModel.Layers (aModel.Layers.First_Index).Output_Data (node_id) :=
                 Base_Neural.Sigmoid
                   (aModel.Layers.First_Element.Output_Data (node_id));
            when Soft_Max_Activation => null;
         end case;

         Put_Line (Routine_Name & "Layer, Node" & Integer'Image (aModel.Layers.First_Index) &
                     "," & Integer'Image (node_id) & " Out_Value: " &
                     Float'Image (aModel.Layers (aModel.Layers.First_Index).Output_Data
                     (node_id)));
      end loop;
      --  Update other layers
      for index in aModel.Layers.First_Index + 1 ..
        aModel.Layers.Last_Index loop
         for node_id in aModel.Layers (index).Nodes.First_Index ..
           aModel.Layers (index).Nodes.Last_Index - 1 loop
            aModel.Layers (index).Nodes (node_id).Features :=
              aModel.Layers (index - 1).Output_Data;

            aModel.Layers (index ).Output_Data (node_id) :=
              aModel.Layers (index).Nodes (node_id).Weights *
              aModel.Layers (index).Nodes (node_id).Features +
              aModel.Layers (index).Nodes (node_id).Bias;

            case aModel.Layers (index).Activation is
               when Identity_Activation => null;
               when ReLu_Activation =>
                  Base_Neural.Rect_LU
                    (aModel.Layers (index ).Output_Data (node_id));
               when Sigmoid_Activation =>
                  aModel.Layers (index ).Output_Data (node_id) := Base_Neural.Sigmoid
                    (aModel.Layers (index).Output_Data (node_id));
               when Soft_Max_Activation => null;
            end case;

            Put_Line (Routine_Name & "Layer, Node" & Integer'Image (index) &
                        "," & Integer'Image (node_id) & " Out_Value: " &
                        Float'Image (aModel.Layers (index).Output_Data
                        (node_id)));
         end loop;

         --           Update_Next_Layer (aModel, index);
      end loop;

   end Forward;

   --  ---------------------------------------------------------------------------

   function Get_Output_Value (aModel : Sequential_Model) return Real_Float_Vector is
   begin
      return aModel.Layers.Last_Element.Output_Data;

   end Get_Output_Value;

   --  ---------------------------------------------------------------------------
   --  Update_Next_Layer sets the
   --     procedure Update_Next_Layer (aModel : in out Sequential_Model;
   --                                  Index  : Positive) is
   --        aLayer     : constant Layer := aModel.Layers (Index);
   --        Next_Layer : Layer := aModel.Layers (Index + 1);
   --        Features   : Real_Float_Vector (1 .. aLayer.Dim);
   --     begin
   --        for index_2 in aLayer.Nodes.First_Index .. aLayer.Nodes.Last_Index loop
   --           Features (index_2) := aLayer.Nodes (index_2).Out_Value;
   --        end loop;
   --
   --        for index_2 in Next_Layer.Nodes.First_Index ..
   --          Next_Layer.Nodes.Last_Index loop
   --           declare
   --              aNode : Node (aLayer.Dim);
   --           begin
   --              for index_3 in aNode.Features.First_Index ..
   --                aNode.Features.Last_Index loop
   --                 aNode.Features (index_3) := Features (index_3);
   --              end loop;
   --           end;
   --        end loop;
   --
   --     end Update_Next_Layer;

   --  ---------------------------------------------------------------------------

end Structure;
