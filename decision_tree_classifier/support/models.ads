--  A collection of Model instances for use with fitting packages
--  from scipy-1.4.1.odr.models.py

with Classifier_Utilities;

package Models is

   function Linear_Function (B, X : Classifier_Utilities.Float_List)
                             return Classifier_Utilities.Float_List;

end Models;
