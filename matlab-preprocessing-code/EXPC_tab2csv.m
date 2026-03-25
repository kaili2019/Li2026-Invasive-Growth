% match parent invasive with surface growth dataset
% 
% Kai Li
% 25 Mar 2026

clear
clc

load("invasive_C3.mat");
load("surface_C3.mat");

%% EXPC_Day_3: match data to get outliers

cc = 1;
for i = 1:length(invasive_C3)
    tmp_name = invasive_C3{i,3}(1:end-1);
    if ~contains(tmp_name,surface_C3(:,3))
%         disp(tmp_name)
        rm_idx_B3W(cc) = i;
        cc = cc+1;
    end
end

%%
cc = 1;
for i = 1:length(surface_C3)
    tmp_name = surface_C3{i,3}+"w";
    if ~contains(tmp_name,invasive_C3(:,3))
%         disp(tmp_name)
        rm_idx_B3S(cc) = i;
        cc = cc+1;
    end
end

%% remove mismatched files

if length(invasive_C3) == 196
    invasive_C3(rm_idx_B3W,:) = [];
end

if length(surface_C3) == 191
    surface_C3(rm_idx_B3S,:) = [];
end

if length(invasive_C3) == 191
    invasive_C3([47,48],:) = [];
end

%%

load("invasive_C6.mat");
load("surface_C6.mat");

%% EXPC_Day_6: match data to get outliers
cc = 1;
for i = 1:length(invasive_C6)
    tmp_name = invasive_C6{i,3}(1:end-1);
    if ~contains(tmp_name,surface_C6(:,3))
%         disp(tmp_name)
        rm_idx_B6W(cc) = i;
        cc = cc+1;
    end
end

%%
cc = 1;
for i = 1:length(surface_C6)
    tmp_name = surface_C6{i,3}+"w";
    if ~contains(tmp_name,invasive_C6(:,3))
%         disp(tmp_name)
        rm_idx_B6S(cc) = i;
        cc = cc+1;
    end
end
%%

if length(invasive_C6) == 46
    invasive_C6(29,:) = [];
end

%% EXPC_Day_4: tabulate data

NC3 = length(invasive_C3);

tab_C3 = cell(NC3,11);

for i = 1:NC3

    % 1. full name or ID
    if contains(invasive_C3{i,3},'796')
        tab_C3{i,1} = "parent";
    else
        tab_C3{i,1} = invasive_C3{i,3}(1:3);
    end
    
    % 2. suphide (Y/N)
    if contains(invasive_C3{i,3},'400S') || contains(invasive_C3{i,3},'750S')
        tab_C3{i,2} = 'Y';
    else
        tab_C3{i,2} = 'N';
    end
    
    tmp_idx = find(contains(surface_C3(:,3),invasive_C3{i,3}(1:end-1)));
    
    % 3. surface area 
    Itmp = surface_C3{tmp_idx,1};
    tab_C3{i,3} = sum(sum(Itmp<0.5));
    
    % 4. washed area
    Jtmp = invasive_C3{i,1};
    tab_C3{i,4} = sum(sum(Jtmp));
    
    % 5. Area ratio
    tab_C3{i,5} = tab_C3{i,4}/tab_C3{i,3};
    
    % 6. Time of trial 
    tab_C3{i,6} = "Nov_11";
    
    % 7. nutrient: 
    if contains(invasive_C3{i,3},'BD50')
        tab_C3{i,7} = "BD50";
    elseif contains(invasive_C3{i,3},'BD75')
        tab_C3{i,7} = "BD75";
    elseif contains(invasive_C3{i,3},'BD100')
        tab_C3{i,7} = "BD100";
    elseif contains(invasive_C3{i,3},'Ox50')
        tab_C3{i,7} = "Ox50";
    elseif contains(invasive_C3{i,3},'Ox75')
        tab_C3{i,7} = "Ox75";
    elseif contains(invasive_C3{i,3},'Ox100')
        tab_C3{i,7} = "Ox100";       
    else
        tab_C3{i,7} = nan;
    end
    
    % 8. SLAD: DNE for this experiment 
    tab_C3{i,8} = nan;

    % 9. Number of days: 3,4 or 6
    tab_C3{i,9} = "day3";
    
    % 10. Experiment type: A,B,C,D or E
    tab_C3{i,10} = "ExpC";

    % 11. Full name
    tab_C3{i,11} = invasive_C3{i,3}(1:end-1);
end

%% EXPC_Day_4: concantenate to a single table 

Id = 1:length(tab_C3);

df_C3 = table;

df_C3.yeast_id = Id';
df_C3.mutant_id = tab_C3(:,1);
df_C3.sulfide = tab_C3(:,2);
df_C3.surface_area = tab_C3(:,3);
df_C3.washed_area = tab_C3(:,4);
df_C3.area_ratio = tab_C3(:,5);
df_C3.time_of_exp = tab_C3(:,6);
df_C3.nutrient = tab_C3(:,7);
df_C3.slad = tab_C3(:,8);
df_C3.day = tab_C3(:,9);
df_C3.exp_type = tab_C3(:,10);
df_C3.full_name = tab_C3(:,11);

%% EXPA_Day_4: write table to csv

writetable(df_C3,'../raw_data/image_data/slad_expC_day3.csv')

%% EXPC_Day_6: tabulate data

NC6 = length(invasive_C6);

tab_C6 = cell(NC6,11);

for i = 1:NC6

    % 1. full name or ID
    if contains(invasive_C6{i,3},'796')
        tab_C6{i,1} = "parent";
    else
        tab_C6{i,1} = invasive_C6{i,3}(1:3);
    end

    % 2. suphide (Y/N)
    if contains(invasive_C3{i,3},'400S') || contains(invasive_C3{i,3},'750S')
        tab_C6{i,2} = 'Y';
    else
        tab_C6{i,2} = 'N';
    end
    
    tmp_idx = find(contains(surface_C6(:,3),invasive_C6{i,3}(1:end-1)));
    
    % 3. surface area 
    Itmp = surface_C6{tmp_idx,1};
    tab_C6{i,3} = sum(sum(Itmp<0.5));
    
    % 4. washed area
    Jtmp = invasive_C6{i,1};
    tab_C6{i,4} = sum(sum(Jtmp));
    
    % 5. Area ratio
    tab_C6{i,5} = tab_C6{i,4}/tab_C6{i,3};
    
    % 6. Time of trial 
    tab_C6{i,6} = "Nov_14";
    
    % 7. nutrient: BD50 or BD75
    if contains(invasive_C6{i,3},'BD50')
        tab_C6{i,7} = "BD50";
    elseif contains(invasive_C6{i,3},'BD75')
        tab_C6{i,7} = "BD75";
    elseif contains(invasive_C6{i,3},'BD100')
        tab_C6{i,7} = "BD100";      
    else
        tab_C6{i,7} = nan;
    end
    
    % 8. SLAD: DNE for this experiment 
    tab_C6{i,8} = nan; 
    
    % 9. Number of days: 3,4 or 6
    tab_C6{i,9} = "day6";
    
    % 10. Experiment type: A,B,C,D or E
    tab_C6{i,10} = "ExpC";

    % 11. Full name
    tab_C6{i,11} = invasive_C6{i,3}(1:end-1);
end

%% EXPA_Day_6: concantenate to a single table 

Id = 1:length(tab_C6);

df_C6 = table;

df_C6.yeast_id = Id';
df_C6.mutant_id = tab_C6(:,1);
df_C6.sulfide = tab_C6(:,2);
df_C6.surface_area = tab_C6(:,3);
df_C6.washed_area = tab_C6(:,4);
df_C6.area_ratio = tab_C6(:,5);
df_C6.time_of_exp = tab_C6(:,6);
df_C6.nutrient = tab_C6(:,7);
df_C6.slad = tab_C6(:,8);
df_C6.day = tab_C6(:,9);
df_C6.exp_type = tab_C6(:,10);
df_C6.full_name = tab_C6(:,11);

%% EXPC_Day_6: write table to csv

writetable(df_C6,'../raw_data/image_data/slad_expC_day6.csv')
