% CheckComponents.m
% Sarah West
% 6/16/22

% Runs PlotWeights, PlotPCTVAR, & PlotMSEPs after a run of PLSR that looked 
% for best number of components with cross validation. Run with
% RunAnalysis.

function [parameters] = CheckComponents(parameters)

      MessageToUser('Checking ', parameters);

      % **** Plot weights 
      parameters = PlotWeights(parameters);
      parameters.fig_weights = parameters.fig; % Rename output figure handle.


      %  **** Plot explanatory & response MSEPs 
      
      % Rename input figure handles, explanatory
      if isfield(parameters, 'fig_MSEPs_explanatory')
          parameters.xfig = parameters.fig_MSEPs_explanatory;

      elseif isfield(parameters, 'xfig')
          parameters = rmfield(parameters, 'xfig');
      end

       % Rename input figure handles, response
      if isfield(parameters, 'fig_MSEPs_response')
          parameters.yfig = parameters.fig_MSEPs_response;

      elseif isfield(parameters, 'yfig')
          parameters = rmfield(parameters, 'yfig');
      end
      % Plot.
      parameters = PlotMSEPs(parameters);

      % Rename output figure handles. 
      parameters.fig_MSEPs_explanatory = parameters.xfig;
      parameters.fig_MSEPs_response = parameters.yfig;


      % **** Plot explanatory & response percent variance 

      
      % Rename input figure handles, explanatory
      if isfield(parameters, 'fig_PCTVARs_explanatory')
          parameters.xfig = parameters.fig_PCTVARs_explanatory;

      elseif isfield(parameters, 'xfig')
          parameters = rmfield(parameters, 'xfig');
      end
      % Rename input figure handles, response
      if isfield(parameters, 'fig_PCTVARs_response')
          parameters.yfig = parameters.fig_PCTVARs_response;

      elseif isfield(parameters, 'yfig')
          parameters = rmfield(parameters, 'yfig');
      end

      % Plot
      parameters = PlotPCTVAR(parameters);

      % Rename output figure handle. 
      parameters.fig_PCTVARs_explanatory = parameters.xfig;
      parameters.fig_PCTVARs_response = parameters.yfig;

                
end 