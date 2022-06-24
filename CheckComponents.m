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


      %  **** Plot explanatory MSEPs (will save only explanatory).
      
      % Rename input figure handles
      if isfield(parameters, 'fig_MSEPs_explanatory')
          parameters.xfig = parameters.fig_MSEPs_explanatory;

      elseif isfield(parameters, 'xfig')
          parameters = rmfield(parameters, 'xfig');
      end
      
      % Plot.
      parameters = PlotMSEPs(parameters);

      % Rename output figure handle. 
      parameters.fig_MSEPs_explanatory = parameters.xfig;


      % **** Plot response MSEPs by variable.

      % Rename input figure handles
      if isfield(parameters, 'fig_MSEPs_response')
          parameters.yfig = parameters.fig_MSEPs_response;

      elseif isfield(parameters, 'yfig')
          parameters = rmfield(parameters, 'yfig');
      end

      % Plot
      parameters = PlotMSEPs_byVar(parameters);

      % Rename output figure handle
      parameters.fig_MSEPs_response = parameters.yfig;


      % **** Plot explanatory percent variance (will save only explanatory)
      % Rename input figure handles
      if isfield(parameters, 'fig_PCTVARs_explanatory')
          parameters.xfig = parameters.fig_PCTVARs_explanatory;

      elseif isfield(parameters, 'xfig')
          parameters = rmfield(parameters, 'xfig');
      end

      % Plot
      parameters = PlotPCTVAR(parameters);

      % Rename output figure handle. 
      parameters.fig_PCTVARs_explanatory = parameters.xfig;


      % **** Plot response percent variance by variable
      parameters.percent_variance_cumulative = false;

      % Rename input figure handles
      if isfield(parameters, 'fig_PCTVARs_response')
          parameters.yfig = parameters.fig_PCTVARs_response;

      elseif isfield(parameters, 'yfig')
          parameters = rmfield(parameters, 'yfig');
      end

      % Plot
      parameters = PlotPCTVAR_byVar(parameters);
      
      % Rename output figure handle.
      parameters.fig_PCTVARs_response = parameters.yfig;


      % **** Plot response percent variance by variable, CUMULATIVE
      % Rename input figure handles
      parameters.percent_variance_cumulative = true;
      if isfield(parameters, 'fig_PCTVARs_response_cumulative')
          parameters.yfig = parameters.fig_PCTVARs_response_cumulative;

      elseif isfield(parameters, 'yfig')
          parameters = rmfield(parameters, 'yfig');
      end

      % Plot
      parameters = PlotPCTVAR_byVar(parameters);
      
      % Rename output figure handle.
      parameters.fig_PCTVARs_response_cumulative = parameters.yfig;
                
end 