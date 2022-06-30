% CheckComponents.m
% Sarah West
% 6/16/22

% Runs PlotWeights, PlotPCTVAR, & PlotMSEPs after a run of PLSR that looked 
% for best number of components with cross validation. Run with
% RunAnalysis.

function [parameters] = CheckComponents(parameters)

      MessageToUser('Checking ', parameters);

      % **** Plot weights 
      % Don't do if user says not to.
      if isfield(parameters, 'plot_weights') && ~parameters.plot_weights
      else
          % Don't do if this is a second level analysis
          if isfield(parameters, 'analysis_level') && parameters.analysis_level == 2
              % Do nothing
          else
              % Plot the weights
              parameters = PlotWeights(parameters);
              parameters.fig_weights = parameters.fig; % Rename output figure handle.
          end
      end
      
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