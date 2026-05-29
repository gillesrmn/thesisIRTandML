# thesisIRTandML

Long scales can sometimes provoke respondent dropout and satisficing (Boateng et al., 2018; Osburn, 2000). This calls for scale abbreviation through methods that accurately capture the original construct's qualities.  There are different frameworks for this, with a main switch from Classical Test Theory (CTT) to Item Response Theory long underway (IRT; Edelen & Reeve, 2007). 

Incoming research has also tfound benefits to shortening scales with machine learning methods (Gonzalez, 2021; Kilmen & Bulut, 2023; Lee et al., 2022). This thesis focuses on Random Forest (RF), Conditional Inference Trees (CIT), and Genetic Algorithms (GA). RF splits splits many datapoints to create mutiple decision trees (Breiman, 2001). CIT extends the logic of RF and adds more formal tests during the splitting process  (Hothorn et al., 2006; Strobl et al., 2009). GA selects and evolves the best sets of items until the most fit solution is found (Koza, 1992). 

IRT, RF, CIT, and GA are compared across two sets of minimum factor loadings (0.3 and 0.4) to determine the effects of an increased loading on the quality of the shortened scale. Each method is the tasked with retaininf 50% and 25% of the total items. 
