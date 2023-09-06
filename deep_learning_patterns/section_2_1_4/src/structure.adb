
with Maths;

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

      --  Initialze weights
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
      aNode.Weights := aLayer.Weights;
      aNode.Bias := aLayer.Bias;
      aNode.Activation :=aLayer.Activation;
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

   --     procedure Connect (Data : Real_Float_Vector; Layer_1 : in out Layer) is
   --        --        Connection : Real_Float_Matrix (Data'Range, 1 .. Layer_1.Dim);
   --     begin
   --        for index in Layer_1.Nodes.First_Index .. Layer_1.Nodes.Last_Index loop
   --           Layer_1.Nodes (index).Data := Data;
   --        end loop;
   --        --        return Connection;
   --
   --     end Connect;

   --  ---------------------------------------------------------------------------

   --     function Connect (Layer_A, Layer_B : Layer) return Real_Float_Matrix is
   --        Connection : Real_Float_Matrix (1 .. Layer_A.Dim, 1 .. Layer_B.Dim);
   --     begin
   --        --  Initialze connection weights
   --        for row in Connection'Range loop
   --           for col in Connection'Range (2) loop
   --              --  Random_Float generates a random number in the range  -1.0 .. 1.0
   --              Connection (row, col) := Maths.Random_Float;
   --           end loop;
   --        end loop;
   --        return Connection;
   --
   --     end Connect;

   --  ---------------------------------------------------------------------------

   procedure Forward (aModel      : in out Sequential_Model;
                      Loss_Method : Loss_Kind) is
      use Real_Float_Arrays;
      use Layer_Packge;
   begin
      for index in aModel.Layers.First_Index .. aModel.Layers.Last_Index - 1 loop
         null;
         --           aModel.Nodes (index + 1).Level :=
         --             aModel.Nodes (index).Level * aModel.Connect_List (index + 1);
      end loop;

   end Forward;

   --  ---------------------------------------------------------------------------

   --     procedure Make_Connections (aModel : in out Sequential_Model) is
   --        use Real_Float_Arrays;
   --        Connect_Inputs : constant Real_Float_Matrix
   --          := Connect (aModel.Input_Data, aModel.Layers.First_Element);
   --     begin
   --        aModel.Connect_List.Append (Connect_Inputs);
   --        for item in aModel.Layers.First_Index ..
   --          aModel.Layers.Last_Index - 1 loop
   --           declare
   --              Connect_Nodes : Real_Float_Matrix
   --                := Connect (aModel.Layers (item), aModel.Layers (item + 1));
   --           begin
   --              aModel.Connect_List.Append (Connect_Nodes);
   --           end;
   --        end loop;
   --
   --        aModel.Layers (aModel.Layers.First_Index).Data :=
   --          aModel.Input_Data *
   --            aModel.Connect_List (aModel.Connect_List.First_Index);
   --
   --     end Make_Connections;

   --  ---------------------------------------------------------------------------

   function Get_Output_Layer (aModel : Sequential_Model) return Layer is
   begin

      return aModel.Layers.Last_Element;

   end Get_Output_Layer;

   --  ---------------------------------------------------------------------------


end Structure;
