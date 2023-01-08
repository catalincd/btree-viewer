#include <bits/stdc++.h>
using namespace std;

const int T = 2;
const int MAX_KEYS = T * 2 - 1;
const int MAX_CHILDREN = T * 2;


struct BNode
{
	int KeysNum;
	int Keys[MAX_KEYS];

	bool Leaf;
	int ChildrenNum;
	BNode* Children[MAX_CHILDREN];

	BNode()
	{
		KeysNum = 0;
		Leaf = false;

		ChildrenNum = 0;
		memset(Keys, 0, sizeof(Keys));
		memset(Children, NULL, sizeof(Children));
	}
};


struct BTree
{
	BNode* root = NULL;

	BTree(){}

	void Insert(int value)
	{
		if(root == NULL)
		{
			root = new BNode();
			root->Leaf = true;
		} 

		if(root->KeysNum == MAX_KEYS)
		{
			BNode* root_copy = root;
			BNode* new_root = new BNode();
			root = new_root;
			new_root->Children[0] = root_copy;
			SplitChild(new_root, 0);
		}

		InsertNonFull(root, value);
		
	}

	void SplitChild(BNode* node, int i)
	{
		BNode* y = node->Children[i];
		BNode* z = new BNode();
		z->Leaf = y->Leaf;

		for(int t=node->KeysNum;t>i;--t)
			node->Keys[t] = node->Keys[t-1];

		node->Keys[i] = y->Keys[T - 1];


		for(int t=node->KeysNum;t>i;--t)
			node->Children[t+1] = node->Children[t];

		node->Children[i+1] = z;
		node->ChildrenNum++;

		for(int t=T;t<MAX_KEYS;t++)
		{
			z->Keys[t-T] = y->Keys[t];
		}

		node->KeysNum++;
		z->KeysNum = T-1;
		y->KeysNum = T-1;

		if(!(y->Leaf))
		{
			for(int t=T;t<MAX_CHILDREN;t++)
			{
				z->Children[t-T] = y->Children[t];
			}
			y->ChildrenNum = T;
			z->ChildrenNum = T;
		}
	}

	void InsertNonFull(BNode* node, int value)
	{
		if(node->Leaf)
		{
			int i = -1;
			while(node->Keys[++i] < value && i < node->KeysNum){}

			for(int t=node->KeysNum;t>i;--t)
				node->Keys[t] = node->Keys[t-1];

			node->Keys[i] = value;
			node->KeysNum++;

			return;
		}
		
		int i;
		for(i=0;i<node->KeysNum;i++)
		{
			if(node->Keys[i] > value)
			{
				break;
			}
		}

		if(node->Children[i]->KeysNum == MAX_KEYS)
		{
			SplitChild(node, i);
			
			if(value > node->Keys[i])
				++i;
		}

		InsertNonFull(node->Children[i], value);
	}


	void Delete(int value)
	{
		Delete(root, value);
	}


	void Delete(BNode* node, int value)
	{
        int i = -1;
		while(node->Keys[++i] < value && i < node->KeysNum){}

        if(node->Leaf)
        {
        	if(i < node->KeysNum && node->Keys[i] == value)
        	{
        		for(int t=i;t<node->KeysNum;++t)
        		{
        			node->Keys[t] = node->Keys[t+1];
        		}
        		node->KeysNum--;
        	}
        	return;
        }


        if(i < node->KeysNum && node->Keys[i] == value)
        {
        	DeleteInternalNode(node, value, i);
        }
        else if(node->Children[i]->KeysNum >= T)
        {
	        Delete(node->Children[i], value);
        }
        else
        {
        	if (i != 0 and i + 1 < node->KeysNum)
        	{
        		if (node->Children[i - 1]->KeysNum >= T)
                    DeleteSibling(node, i, i - 1);
                else if(node->Children[i + 1]->KeysNum >= T)
                    DeleteSibling(node, i, i + 1);
                else
                    DeleteMerge(node, i, i + 1);
        	}
            else if(i == 0)
            {
            	if(node->Children[i + 1]->KeysNum >= T)
                    DeleteSibling(node, i, i + 1);
                else
                    DeleteMerge(node, i, i + 1);
            }
           	else if (i == node->KeysNum)
           	{
                if(node->Children[i - 1]->KeysNum >= T)
                    DeleteSibling(node, i, i - 1);
                else
                    DeleteMerge(node, i, i - 1);
           	}

            Delete(node->Children[i], value);
        }  
    }

    void DeleteInternalNode(BNode* node, int value, int i)
    {
        if(node->Leaf)
        {
        	if(node->Keys[i] == value)
            {
            	for(int t=i;t<node->KeysNum;++t)
        		{
        			node->Keys[t] = node->Keys[t+1];
        		}
        		node->KeysNum--;
            }
            return;
        }

        if(node->Children[i]->KeysNum >= T)
        {
        	node->Keys[i] = DeletePredecessor(node->Children[i]);
            return;
        }
        else if(node->Children[i + 1]->KeysNum >= T)
        {
        	node->Keys[i] = DeleteSuccesor(node->Children[i + 1]);
            return;
        }
        else
        {
        	DeleteMerge(node, i, i + 1);
            DeleteInternalNode(node->Children[i], value, T-1);
        }
    }

    int DeletePredecessor(BNode* node)
    {
    	if(node->Leaf)
    	{
    		int popped = node->Keys[node->KeysNum-1];
    		node->KeysNum--;
    		return popped;
    	}

        int n = node->KeysNum-1;

        if(node->Children[n]->KeysNum >= T)
            DeleteSibling(node, n + 1, n);
        else
            DeleteMerge(node, n, n + 1);

        DeletePredecessor(node->Children[n]);

        return -1;
    }

    int DeleteSuccesor(BNode* node)
    {
    	if(node->Leaf)
    	{
    		int popped = node->Keys[0];

    		for(int i=1;i<node->KeysNum;i++)
    			node->Keys[i-1] = node->Keys[i];

    		node->KeysNum--;
    		return popped;
    	}

        int n = node->KeysNum-1;

        if(node->Children[1]->KeysNum >= T)
            DeleteSibling(node, 0, 1);
        else
            DeleteMerge(node, 0, 1);

        DeleteSuccesor(node->Children[0]);

        return -1;
    }
        

    void DeleteSibling(BNode* node, int i, int j)
    {
    	BNode* cnode = node->Children[i];
        if(i < j)
        {
        	BNode* rsnode = node->Children[j];
            cnode->Keys[cnode->KeysNum++] = node->Keys[i];
            node->Keys[i] = rsnode->Keys[0];

            if(!(rsnode->Leaf))
            {
            	cnode->Children[cnode->ChildrenNum++] = rsnode->Children[0];
               	
               	for(int t=1;t<rsnode->ChildrenNum;t++)
               	{
               		rsnode->Children[t-1] = rsnode->Children[t];
               	}

               	rsnode->ChildrenNum--;
            }

            for(int t=1;t<rsnode->KeysNum;t++)
    			rsnode->Keys[t-1] = rsnode->Keys[t];

    		rsnode->KeysNum--;
        }
        else
        {
        	BNode* lsnode = node->Children[j];

            for(int t=cnode->KeysNum;t>0;t--)
            	cnode->Keys[t] = cnode->Keys[t-1];

            cnode->Keys[0] = node->Keys[i-1];
            node->Keys[i - 1] = lsnode->Keys[--lsnode->KeysNum];


            if(lsnode->ChildrenNum > 0)
            {
            	for(int t=cnode->ChildrenNum;t>0;--t)
            	{
            		cnode->Children[t] = cnode->Children[t-1];
            	}
            	cnode->Children[0] = lsnode->Children[--(lsnode->ChildrenNum)];
            }
        }
    }

    void DeleteMerge(BNode* node, int i, int j)
    {
        BNode* cnode = node->Children[i];
        BNode* newNode;

        if(j > i)
        {
        	BNode* rsnode = node->Children[j];
            cnode->Keys[cnode->KeysNum++] = node->Keys[i];           

            for(int k=0;k<rsnode->KeysNum;i++)
            {
            	cnode->Keys[cnode->KeysNum++] = rsnode->Keys[k];

                if(rsnode->ChildrenNum > 0)
                {
                    cnode->Children[cnode->ChildrenNum++] = rsnode->Children[k];
                }
            }

            if(rsnode->ChildrenNum > 0)
            {
                cnode->Children[cnode->ChildrenNum++] = rsnode->Children[--(rsnode->ChildrenNum)];
            }

            newNode = cnode;
            
            for(int t=i;t<node->KeysNum;t++)
            {
            	node->Keys[t] = node->Keys[t+1];
            }
            node->KeysNum--;

            for(int t=j;t<node->ChildrenNum;t++)
            {
            	node->Children[t] = node->Children[t+1];
            }
            node->ChildrenNum--;
        }
        else
        {
        	BNode* lsnode = node->Children[j];
            lsnode->Keys[lsnode->KeysNum++] = node->Keys[j];

            for(int t=0;t<cnode->KeysNum;t++)
            {
            	lsnode->Keys[lsnode->KeysNum++] = cnode->Keys[t];

            	if(lsnode->ChildrenNum > 0)
                {
                    lsnode->Children[lsnode->ChildrenNum++] = cnode->Children[--(cnode->ChildrenNum)];
                }
            }


            if(lsnode->ChildrenNum > 0)
            {
                lsnode->Children[lsnode->ChildrenNum++] = cnode->Children[--(cnode->ChildrenNum)];
            }

            newNode = lsnode;
            for(int t=j;t<node->KeysNum;t++)
            {
            	node->Keys[t] = node->Keys[t+1];
            }
            node->KeysNum--;

            for(int t=i;t<node->ChildrenNum;t++)
            {
            	node->Children[t] = node->Children[t+1];
            }
            node->ChildrenNum--;
        }

        if(node == root && node->KeysNum == 0)
            root = newNode;
    }

	void Print()
	{
		Print(root);
		std::cout<<std::endl;
	}

	void Print(BNode* node)
	{
		for(int i=0;i<node->KeysNum;i++)
		{
			if(!(node->Leaf))
			{
				Print(node->Children[i]);
			}
			std::cout<<node->Keys[i]<<' ';
		}
		if(!(node->Leaf))
		{
			Print(node->Children[node->KeysNum]);
		}
	}
};


int main()
{
	BTree tree1;

	tree1.Insert(5);
	tree1.Insert(10);
	tree1.Insert(3);
	tree1.Insert(1);
	tree1.Delete(1);
	tree1.Insert(15);
	tree1.Insert(20);
	tree1.Delete(10);
	tree1.Insert(4);

	/// 3 4 5 15 20 
	
	tree1.Print();

	return 0;
}