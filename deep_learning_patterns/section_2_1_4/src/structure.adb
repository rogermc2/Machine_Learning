
with Maths;

with Stochastic_Optimizers;

package body Structure is

   procedure Forward (aModel : in out Sequential_Model; Loss_Method : Loss_Kind);

   --  ---------------------------------------------------------------------------

   procedure Add_Layer (aModel     : in out Sequential_Model;
                        Num_Nodes  : Positive;
                        Input_Data : Real_Float_Vector;
                        Activation : Activation_Kind := Identity_Activation) is
      thisLayer : Layer (Num_Nodes);
   begin
      thisLayer.Activation := Activation;
      for index in 1 .. Num_Nodes loop
         Add_Node (thisLayer, Input_Data);
      end loop;
      aModel.Layers.Append (thisLayer);

   end Add_Layer;

   --  ---------------------------------------------------------------------------


   procedure Add_Layer (aModel    : in out Sequential_Model; Num_Nodes : Positive;
                       Activation : Activation_Kind := Identity_Activation) is
      thisLayer  : Layer (Num_Nodes);
      Prev_Layer : Layer := aModel.Layers.Last_Element;
      Prev_Nodes : Node_List := Prev_Layer.Nodes;
   begin
      thisLayer.Activation := Activation;
      for index in 1 .. Num_Nodes loop
         for prev in Prev_Nodes.First_Index .. Prev_Nodes.Last_Index loop
            null;
--              Add_Node (thisLayer, Prev_Layer.Nodes (prev).o);
         end loop;
      end loop;
      aModel.Layers.Append (thisLayer);

   end Add_Layer;

   --  ---------------------------------------------------------------------------

   procedure Add_Node (aLayer     : in out Layer; Data : Real_Float_Vector;
                       Activation : Activation_Kind := Identity_Activation) is
      aNode : Node (Data'Length);
   begin
      aNode.Data := Data;
      aNode.Activation := Activation;
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

   procedure Connect (Data : Real_Float_Vector; Layer_1 : in out Layer) is
--        Connection : Real_Float_Matrix (Data'Range, 1 .. Layer_1.Dim);
   begin
      for index in Layer_1.Nodes.First_Index .. Layer_1.Nodes.Last_Index loop
         Layer_1.Nodes (index).Data := Data;
      end loop;
--        return Connection;

   end Connect;

   --  ---------------------------------------------------------------------------

   function Connect (Layer_A, Layer_B : Layer) return Real_Float_Matrix is
      Connection : Real_Float_Matrix (1 .. Layer_A.Dim, 1 .. Layer_B.Dim);
   begin
      --  Initialze connection weights
      for row in Connection'Range loop
         for col in Connection'Range (2) loop
            --  Random_Float generates a random number in the range  -1.0 .. 1.0
            Connection (row, col) := Maths.Random_Float;
         end loop;
      end loop;
      return Connection;

   end Connect;

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
