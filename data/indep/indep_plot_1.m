%% importing data
transitions = dlmread('indep_trans_1.csv', ',', 1, 0);
% convert 0 values to NaNs, as these don't plot
transitions(transitions==0)=nan;
threads = [1:19];

%% plotting lines

semilogy(threads, transitions(:, 1), 'ko-')
hold on
semilogy(threads, transitions(:, 2), 'kx-')
semilogy(threads, transitions(:, 3), 'ks-')
semilogy(threads, transitions(:, 4), 'bo-')
semilogy(threads, transitions(:, 5), 'bx-')
semilogy(threads, transitions(:, 6), 'bs-')
hold off
grid on

%% adding axis labels
% legend doesn't fit, so just refer to other figure for key
set(gca, 'FontName', 'Palatino Linotype', 'FontSize', 14);
xlabel('Number of Threads');
ylabel('Number of Transitions')

%% output pdf file
h = figure(1);
h.Units = 'centimeters'; % set figure position to cm
%h.Position(2) = [h.Position(2)-9]; % set figure position bevore resize
h.Position([3:4]) = [10,8]; % resize figure
Plot2LaTeX( h, 'indep_trans_fig1' )