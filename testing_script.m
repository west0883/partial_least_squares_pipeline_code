%beta_diag = T' * U;
%beta_diag = X_latentvariables' * Y_latent_variables; 
beta_diag = PLSR_results.XS' * PLSR_results.YS;

%beta_PLSR = pinv(P) * beta_diag * C';

beta_diag = PLSR_results.XS' * PLSR_results.YS;
beta_PLSR = pinv(PLSR_results.XL') * beta_diag * PLSR_results.YL'; 

%XS = X0*W

XS = PLSR_results.XS;
YS = PLSR_results.YS;
XL = PLSR_results.XL;
YL = PLSR_results.YL;
W = PLSR_results.stats.W;
BETA = PLSR_results.BETA;
Y = dataset_info.responseVariables;
X = dataset_info.explanatoryVariables;

X_sigma = dataset_info.zscoring.explanatoryVariables.sigma;

beta_diag = XS' * YS;
beta_PLSR = pinv(XL') * beta_diag * YL'; 


X0 = XS * XL';
Y0 = XS * YL';
diffX = X - X0;
diffY = Y - Y0;

Wy = YS'/Y';



R = X' * Y;
[U,S,V] = SVD(X,'econ');

% Matlab's version?
BETA_calc = pinv(W') * beta_diag * Wy; 

holder = NaN(32);
holder(parameters.indices) = BETA(2:end,:);

figure; imagesc(holder); title('BETA'); colorbar;

holder(parameters.indices) = BETA_calc;
figure; imagesc(holder); title('BETA_calc'); colorbar;
%BETA_sigma_adjusted = BETA(2:end,:) .* X_sigma'; 

%beta_PLSR_sigma_adjusted = beta_PLSR .* X_sigma';


