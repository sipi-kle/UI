	public static void main(String[] args) throws FileNotFoundException {
		String[] fileNames= {"labyrinth_1.txt","labyrinth_2.txt","labyrinth_3.txt","labyrinth_4.txt","labyrinth_5.txt","labyrinth_6.txt","labyrinth_7.txt","labyrinth_8.txt","labyrinth_9.txt","labyrinth_10.txt","labyrinth_11.txt","labyrinth_12.txt"};
	for(int i=0;i<fileNames.length;i++) {
		ArrayList<int[]> arr =new ArrayList<int[]>();
		File file = new File(fileNames[i]);
		Scanner sc=new Scanner(file);
		while (sc.hasNextLine()) {
			
			String[] line=sc.nextLine().split(",");
			int[] tmp=new int[line.length];
			for(int j=0;j<line.length;j++) {
				tmp[j]=Integer.parseInt(line[j]);
			}
			arr.add(tmp);
		}
		
		int[][] lab=new int[arr.size()][arr.get(0).length];
		for(int j=0;j<lab.length;j++) {
			lab[j]=arr.get(j);
		}
		
		executeCommand(lab);
		System.out.println("------------------------"+fileNames[i]+"--------------------------------------------------------");
	}
		
	}
	
	public static void executeCommand(int[][] labirint) {
		for(int i=0;i<labirint.length;i++) {
			for(int j=0;j<labirint[i].length;j++) {
				System.out.print(labirint[i][j]);
			}
			System.out.println();
		}
	}