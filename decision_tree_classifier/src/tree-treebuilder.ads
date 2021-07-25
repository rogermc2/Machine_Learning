--  The TreeBuilder recursively builds a Tree object from training samples,
--  using a Splitter object for splitting internal nodes and assigning
--  values to leaves.
--  The TreeBuilder controls the various stopping criteria and the node
--  splitting evaluation order, e.g. depth-first or best-first.

with Classifier_Utilities;
with Tree;
with Validation;

package Tree.TreeBuilder is

   procedure Build (Self : Validation.Attribute_List; aTree : in out Tree_Data;
          X, Y : Sample_Matrix;
          Sample_Weight : Classifier_Utilities.Float_Array;
          X_Idx_Sorted : Classifier_Utilities.Integer_Array);

end Tree.TreeBuilder;
