% CheckComponents.m
% Sarah West
% 6/16/22

% Runs PlotWeights, PlotPCTVAR, PlotMSEPs, PlotBICs after a run of PLSR that looked 
% for best number of components with cross validation. Run with
% RunAnalysis.

function [parameters] = CheckComponents(parameters)

      MessageToUser('Checking ', parameters);

      % Define number of sources (if is different for each mouse)
      if isfield(parameters, 'define_number_of_sources') && parameters.define_number_of_sources
          corr_num = size(parameters.results.Cov, 1);

          % (found this with a quadratic equation)
          parameters.number_of_sources = 0.5 * (1 + sqrt(8 * corr_num + 1 ));

          parameters.indices = find(tril(ones(parameters.number_of_sources), -1));
      end 

      % **** Plot weights 
      % Don't do if user says not to.
      if isfield(parameters, 'plot_weights') && ~parameters.plot_weights
          % Do nothing
      else
     
          % Plot the weights
          parameters = PlotWeights(parameters);
          parameters.fig_weights = parameters.fig; % Rename output figure handle.
      end
      
      %  **** Plot explanatory & response MSEPs 
      % If user says so
      if isfield(parameters, 'plot_MSEPs') && parameters.plot_MSEPs
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
      end

      % **** Plot BICs 
      % If user says so
      if isfield(parameters, 'plot_BICs') && parameters.plot_BICs
          % Rename input figure handles, explanatory
          if isfield(parameters, 'fig_BICs_explanatory')
              parameters.xfig = parameters.fig_BICs_explanatory;
    
          elseif isfield(parameters, 'xfig')
              parameters = rmfield(parameters, 'xfig');
          end
    
           % Rename input figure handles, response
          if isfield(parameters, 'fig_BICs_response')
              parameters.yfig = parameters.fig_BICs_response;
    
          elseif isfield(parameters, 'yfig')
              parameters = rmfield(parameters, 'yfig');
          end
          % Plot.
          parameters = PlotBICs(parameters);
    
          % Rename output figure handles. 
          parameters.fig_BICs_explanatory = parameters.xfig;
          parameters.fig_BICs_response = parameters.yfig;
      end


      % **** Plot explanatory & response percent variance 

      % If user says so.
      if isfield(parameters, 'plot_percentVars') && parameters.plot_percentVars
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
                
end 