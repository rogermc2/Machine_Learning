--  The TreeBuilder recursively builds a Tree object from training samples,
--  using a Splitter object for splitting internal nodes and assigning
--  values to leaves.
--  The TreeBuilder controls the various stopping criteria and the node
--  splitting evaluation order, e.g. depth-first or best-first.

--  A tree can be explained by two entities, decision nodes and leaves.
--  The leaves are the decisions or the final outcomes.
--  The decision nodes are where the data is split.
--  In tree structures, leaves represent class labels and branches represent
--  conjunctions of features that lead to those class labels.

--  Algorithm from https://towardsdatascience.com/decision-tree-in-machine-learning-e380942a4c96
--  1. Get list of rows (dataset) which are taken into consideration for making decision tree (recursively at each nodes).
--  2. Calculate uncertanity of our dataset or Gini impurity or how much our data is mixed up etc.
--  3. Generate list of all question which needs to be asked at that node.
--  4. Partition rows into True rows and False rows based on each question asked.
--  5. Calculate information gain based on gini impurity and partition of data from previous step.
--  6. Update highest information gain based on each question asked.
--  7. Update best question based on information gain (higher information gain).
--  8. Divide the node on best question. Repeat again from step 1 again until we get pure node (leaf nodes).

with Ada.Containers.Multiway_Trees;
with Tree;

package body Tree.TreeBuilder is

   function Gini (Rows : Validation.Attribute_List) return Float;

   procedure Build (Self          : Validation.Attribute_List; aTree : in out Tree_Data;
                    X, Y          : Sample_Matrix;
                    Sample_Weight : Classifier_Utilities.Float_Array;
                    X_Idx_Sorted  : Classifier_Utilities.Integer_Array) is
      use Tree_Package;
      --  Tree.Capacity must be set in calling routine when Tree is declared.
      --  Initial_Capacity : Integer := 2047;  --  corresponds to Max_Depth of 10
      Curs        : Cursor := Root (aTree.Nodes);
      Root_Node   : Tree_Node (Integer_Type);
      I_Node      : Tree_Node (Integer_Type);
      F_Node      : Tree_Node (Float_Type);
   begin
      --        if Tree.Max_Depth <= 10 then
      --           Initial_Capacity := 2 ** (Tree.Max_Depth + 1) - 1;
      --        end if;

      Root_Node.Num_Node_Samples := X'Length;
      aTree.Nodes.Insert_Child (Parent   => Curs,
                                Before   => No_Element,
                                New_Item => Root_Node,
                                Position => Curs);
      for Curs in aTree.Nodes.Iterate_Children (aTree.Nodes.Root) loop
         I_Node := Element (Curs);
      end loop;

   end Build;

   --  -------------------------------------------------------------------------

   --   Find_Best_Split finds the best question to ask by iterating over every
   --   feature / value  and calculating the information gain.
   procedure Find_Best_Split (Rows : Validation.Attribute_List) is

    Best_Gain : Float := 0.0;  -- keep track of the best information gain
--      best_question = None;  -- keep train of the feature / value that produced it
    Current_Uncertainty : Float := 0.0;
   begin
      Current_Uncertainty := Gini (Rows);

   end Find_Best_Split;

   --  -------------------------------------------------------------------------
--  Gini calculates the Gini Impurity for a list of rows.
--
--      There are a few different ways to do this, this one seems
--      the most concise:
--      https://en.wikipedia.org/wiki/Decision_tree_learning#Gini_impurity

   function Gini (Rows : Validation.Attribute_List) return Float is
      Data_Length    : Integer := Integer (Rows.Length);
      Counts         : array (1 .. Data_Length) of Integer; --  := Class_Counts (Rows);
      Impurity       : Float := 1.0;
      Prob_Of_Lable  : Float := 0.0;
   begin
    For Lable in 1 .. Data_Length loop
        Prob_Of_Lable := Float (Counts (Lable)) / Float (Data_Length);
        Impurity := Impurity - Prob_Of_Lable ** 2;
    end loop;
    return Impurity;
   end Gini;

   --  -------------------------------------------------------------------------

end Tree.TreeBuilder;
