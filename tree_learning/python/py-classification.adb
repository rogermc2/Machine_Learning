
with Interfaces.C;

with Tree;

package body Py.Classification is

   function Create_Classifier_Type (Name : Classifier) return Handle is
      use Interfaces.C;
      Module_Name   : aliased char_array := "sample_module" & Nul;
      aClassifier   : Classifier
        (Tree.Integer_Type, Tree.Integer_Type, Tree.Integer_Type);
   begin
      return aClassifier;
   end Create_Classifier_Type;

end Py.Classification;
