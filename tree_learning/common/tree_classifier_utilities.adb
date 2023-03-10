
with Ada.Containers.Ordered_Sets;
with Ada.Numerics.Elementary_Functions;
with Ada.Text_IO; use Ada.Text_IO;

with Base_Decision_Tree;
with Encode_Utils;
with Weights;

package body Tree_Classifier_Utilities is

   use ML_Types;

   package Int_Sets is new Ada.Containers.Ordered_Sets (Integer);
   use ML_Types.Value_Data_Package;
   package Value_Sets is new
     Ada.Containers.Ordered_Sets (ML_Types.Value_Record);
   use Weights.Weight_Lists_3D_Package;
   package Weight_Sets is new
     Ada.Containers.Ordered_Sets (Float);

   package Float_IO is new Ada.Text_IO.Float_IO (Num => Float);

   --  -------------------------------------------------------------------------

   function Count_Samples (aClassifier : Base_Decision_Tree.Classifier)
                           return Natural is
      use Ada.Containers;
      use Tree;
      use Nodes_Package;
      Nodes       : constant Nodes_Package.Tree :=
                      aClassifier.Attributes.Decision_Tree.Nodes;
      Num_Samples : Natural := 0;

      procedure Add (Curs : Nodes_Package.Cursor) is
         Node : constant Tree_Node := Element (Curs);
      begin
         if Curs /= Nodes.Root then
            Num_Samples := Num_Samples + Node.Num_Node_Samples;
         end if;
      end Add;

   begin
      Iterate (Nodes, Add'Access);
      return Num_Samples / Integer (Nodes.Node_Count - 2);

   end Count_Samples;

   --  -------------------------------------------------------------------------

   function Traverse_Tree (Current_Node : Tree.Tree_Cursor)
                           return Tree.Tree_Cursor is
      use Ada.Containers;
      use Tree.Nodes_Package;
      Parent_Node : constant Tree.Tree_Cursor := Parent (Current_Node);
      Next_Node   : Tree.Tree_Cursor;
   begin
      if not Is_Leaf (Current_Node) then
         if Current_Node = First_Child (Parent_Node) then
            if Child_Count (Parent_Node) > 1 then
               Next_Node := Next_Sibling (First_Child (Current_Node));
            end if;
         end if;
      end if;

      return Next_Node;

   end Traverse_Tree;

   --  -------------------------------------------------------------------------

end Tree_Classifier_Utilities;
