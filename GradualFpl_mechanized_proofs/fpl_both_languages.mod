module fpl_both_languages.

accumulate fpl. 
accumulate gradual_fpl. 

% As Abella can load only one specification .mod, we have created fpl_both_languages.mod to accumulates both.
% conservative_extension.thm loads "fpl_both_languages" to handle both the original fpl and its gradualized version. 