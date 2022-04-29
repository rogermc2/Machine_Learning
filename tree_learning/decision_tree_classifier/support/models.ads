--  A collection of Model instances for use with fitting packages
--  from scipy-1.4.1.odr.models.py

with Classifier_Types;

package Models is

   function Linear_Function (B, X : Classifier_Types.Float_List)
                             return Classifier_Types.Float_List;

end Models;
